/*
 * Same-Gnome: the game.
 * (C) 1997 the Free Software Foundation
 *
 * Author: Miguel de Icaza.
 *         Federico Mena.
 *         Horacio Peña.
 *
 * The idea is originally from KDE's same game program.
 *
 */
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <dirent.h>

#include <config.h>
#include <gnome.h>
#include <libgnomeui/gnome-window-icon.h>
#include <gdk_imlib.h>


#define STONE_SIZE 40
#define STONE_COLS  15
#define STONE_LINES 10
#define GAME_EVENTS (GDK_EXPOSURE_MASK              |\
		     GDK_BUTTON_PRESS_MASK          |\
		     GDK_ENTER_NOTIFY_MASK          |\
		     GDK_LEAVE_NOTIFY_MASK          |\
		     GDK_POINTER_MOTION_MASK)

static GtkWidget *pref_dialog, *scorew;
static GtkWidget *app, *draw_area, *vb, *appbar;
static GdkImlibImage *image;
static GdkPixmap *stones, *mask;
static int tagged_count = 0;
static int ball_timeout_id = -1;
static int old_x = -1, old_y = -1;
static int score;
static gchar *scenario;
static gint restarted;
static gint debugging;

static struct {
	char *scenario;
	int make_it_default;
} selected_scenario = {0,0};

static struct ball {
	int color;
	int tag;
	int frame;
} field [STONE_COLS][STONE_LINES];

static int nstones;
static int ncolors;
static int sync_stones = 0;

/* This is used only while parsing arguments.  */
static gchar *fname;

#define mapx(x) (x)
#define mapy(y) (STONE_LINES-1-(y))

static void
draw_ball (int x, int y)
{
	int bx, by;

	if (field [x][y].color){
		by = STONE_SIZE * (field [x][y].color - 1);
		bx = STONE_SIZE * (field [x][y].frame);
		
		gdk_draw_pixmap (draw_area->window,
				 draw_area->style->black_gc, stones,
				 bx, by, x * STONE_SIZE, y * STONE_SIZE,
				 STONE_SIZE, STONE_SIZE);
	} else {
		gdk_window_clear_area (draw_area->window, x * STONE_SIZE, y * STONE_SIZE,
				       STONE_SIZE, STONE_SIZE);
	}
}

static void
paint (GdkRectangle *area)
{
	int x1, y1, x2, y2, x, y;
	
	x1 = area->x / STONE_SIZE;
	y1 = area->y / STONE_SIZE;
	x2 = (area->x + area->width) / STONE_SIZE;
	y2 = (area->y + area->height) / STONE_SIZE;

	for (x = x1; x <= x2; x++){
		for (y = y1; y <= y2; y++){
			draw_ball (x, y);
		}
	}
}

static void
untag_all ()
{
	int x, y;

	for (x = 0; x < STONE_COLS; x++)
		for (y = 0; y < STONE_LINES; y++){
			field [x][y].tag   = 0;
			if (sync_stones){
				field [x][y].frame = 0;
				draw_ball (x, y);
			}
		}
}

static int
flood_fill (int x, int y, int color)
{
	int c = 0;
	
	if (field [x][y].color != color)
		return c;
	
	if (field [x][y].tag)
		return c;

	c = 1;
	field [x][y].tag = 1;
	
	if (x+1 < STONE_COLS)
		c += flood_fill (x+1, y, color);
	if (x)
		c += flood_fill (x-1, y, color);
	if (y+1 < STONE_LINES)
		c += flood_fill (x, y+1, color);
	if (y)
		c += flood_fill (x, y-1, color);
	return c;
}

static int
move_tagged_balls (void *data)
{
	int x, y;
	
	for (x = 0; x < STONE_COLS; x++)
		for (y = 0; y < STONE_LINES; y++){
			if (!field [x][y].tag)
				continue;
			field [x][y].frame = (field [x][y].frame + 1) % nstones;
			draw_ball (x, y);
		}
	gdk_flush ();
	return 1;
}

static void
disable_timeout ()
{
	if (ball_timeout_id != -1){
		gtk_timeout_remove (ball_timeout_id);
		ball_timeout_id = -1;
	}
}

static void
mark_balls (int x, int y)
{
	if (x == old_x && y == old_y)
		return;
	old_x = x;
	old_y = y;

	untag_all ();
	disable_timeout ();
	if (!field [x][y].color)
		return;
	
	tagged_count = flood_fill (x, y, field [x][y].color);
	
	if (tagged_count > 1)
		ball_timeout_id = gtk_timeout_add (100, move_tagged_balls, 0);
}

static void
compress_column (int x)
{
	int y, ym;
	
	for (y = STONE_LINES - 1; y >= 0; y--){
		if (!field [mapx(x)][mapy(y)].tag)
			continue;
		for (ym = y; ym < STONE_LINES - 1; ym++)
			field [mapx(x)][mapy(ym)] = field [mapx(x)][mapy(ym+1)];
		field [mapx(x)][mapy(ym)].color = 0;
		field [mapx(x)][mapy(ym)].tag   = 0;
	}
}

static void
compress_y ()
{
	int x;

	for (x = 0; x < STONE_COLS; x++)
		compress_column (x);
}

static void
copy_col (int dest, int src)
{
	int y;
	
	for (y = 0; y < STONE_LINES; y++)
		field [mapx(dest)][mapy(y)] = field [mapx(src)][mapy(y)];
}

static void
clean_last_col ()
{
	int y;

	for (y = 0; y < STONE_LINES; y++){
		field [mapx(STONE_COLS-1)][mapy(y)].color = 0;
		field [mapx(STONE_COLS-1)][mapy(y)].tag   = 0;
	}
}

static void
compress_x ()
{
	int x, xm, l;

	for (x = 0; x < STONE_COLS; x++){
		for (l = STONE_COLS; field [mapx(x)][mapy(0)].color == 0 && l; l--){
			for (xm = x; xm < STONE_COLS-1; xm++)
				copy_col (xm, xm+1);
			clean_last_col ();
		} 
	}
}

static void
set_score (int new_score)
{
	char b [20];
	
	score = new_score;
	sprintf (b, "%.5d", score);
	gtk_label_set (GTK_LABEL(scorew), b);
}

static void
show_scores ( gchar *title, guint pos )
{
	gnome_scores_display (title, "same-gnome", NULL, pos);
}

static void
game_top_ten_callback(GtkWidget *widget, gpointer data)
{
	show_scores(_("The Same Gnome"), 0);
}

static void
end_of_game (char *title)
{
	int pos;

	pos = gnome_score_log(score, NULL, TRUE);
	show_scores(title, pos);
}

static void
check_game_over (void)
{
	int cleared=1;
	int x,y;
	
	for(x = 0; x < STONE_COLS; x++)
		for(y = 0 ; y < STONE_LINES; y++) {
			if (!field [x][y].color)
				continue;
			cleared = 0;
			if(x+1 < STONE_COLS) 
				if(field[x][y].color == field[x+1][y].color)
					return;
			if(y+1 < STONE_LINES) 
				if(field[x][y].color == field[x][y+1].color)
					return;
		}
	if (cleared){
		set_score (score+1000);
		end_of_game (_("The Same Gnome"));
	}
	else
		end_of_game(_("The Same Gnome"));
}

static void
kill_balls (int x, int y)
{
	if (!field [x][y].color)
		return;
	
	if (tagged_count < 2)
		return;

	set_score (score + (tagged_count - 2) * (tagged_count - 2));
	compress_y ();
	compress_x ();
	gtk_widget_draw (draw_area, NULL);
	check_game_over ();
}

static gint
area_event (GtkWidget *widget, GdkEvent *event, void *d)
{
	switch (event->type){
	case GDK_EXPOSE: {
		GdkEventExpose *e = (GdkEventExpose *) event;
		paint (&e->area);
		return TRUE;
	}
	
	case GDK_BUTTON_PRESS: {
		int x, y;
		gtk_widget_get_pointer (widget, &x, &y);
		kill_balls (x / STONE_SIZE, y / STONE_SIZE);
		old_x = -1;
		old_y = -1;
	}

	case GDK_ENTER_NOTIFY:
	case GDK_MOTION_NOTIFY: {
		int x, y;
		
		gtk_widget_get_pointer (widget, &x, &y);
		mark_balls (x / STONE_SIZE, y / STONE_SIZE);
		return TRUE;
	}
	
	case GDK_LEAVE_NOTIFY:
		old_x = -1;
		old_y = -1;
		disable_timeout ();
		untag_all ();
		return TRUE;

	default:
		return FALSE;
	}
}

static void
fill_board (void)
{
	int x, y;

	for (x = 0; x < STONE_COLS; x++)
		for (y = 0; y < STONE_LINES; y++){
			field [x][y].color = 1 + (rand () % ncolors);
			field [x][y].tag   = 0;
			field [x][y].frame = sync_stones ? 0 : (rand () % nstones);
		}
}

static void
new_game (void)
{
	fill_board ();
	set_score (0);
	gtk_widget_draw (draw_area, NULL);
}

static void
configure_sync (char *fname)
{
	if (strstr (fname, "-sync.png"))
		sync_stones = 1;
	else
		sync_stones = 0;
}

static void
load_scenario (char *fname)
{
	char *tmp, *fn;
        GdkColor bgcolor;
        GdkImage *tmpimage;
    
	tmp = g_strconcat ( "same-gnome/", fname, NULL);

	fn = gnome_unconditional_pixmap_file ( tmp );
	g_free( tmp );

	if (!g_file_exists (fn)) {
		printf (_("Could not find the \'%s\' theme for SameGnome\n"), fn);
		exit (1);
	}

	if (scenario)
		g_free (scenario);

	scenario = g_strdup(fname);

	configure_sync (fname);

	if (image)
		gdk_imlib_destroy_image (image);

	image = gdk_imlib_load_image (fn);
	gdk_imlib_render (image, image->rgb_width, image->rgb_height);

	stones = gdk_imlib_move_image (image);
	mask = gdk_imlib_move_mask (image);

        tmpimage = gdk_image_get(stones, 0, 0, 1, 1);
        bgcolor.pixel = gdk_image_get_pixel(tmpimage, 0, 0);
        gdk_window_set_background (draw_area->window, &bgcolor);
        gdk_image_destroy(tmpimage);
  
	g_free( fn );

	nstones = image->rgb_width / STONE_SIZE;
/*	ncolors = image->rgb_height / STONE_SIZE; */
	ncolors = 3;


	gtk_widget_draw (draw_area, NULL);
}

static void
set_selection (GtkWidget *widget, void *data)
{
	selected_scenario.scenario = data;
}

static void
set_selection_def (GtkWidget *widget, gpointer *data)
{
	selected_scenario.make_it_default = GTK_TOGGLE_BUTTON (widget)->active;
}

static void
create_same_board (char *fname)
{
	gtk_widget_push_visual (gdk_imlib_get_visual ());
	gtk_widget_push_colormap (gdk_imlib_get_colormap ());

	draw_area = gtk_drawing_area_new ();

        gtk_widget_pop_colormap ();
	gtk_widget_pop_visual ();

	gtk_widget_set_events (draw_area, gtk_widget_get_events (draw_area) | GAME_EVENTS);

	gtk_box_pack_start_defaults (GTK_BOX(vb), draw_area);
	gtk_widget_realize (draw_area);
  
	gtk_widget_show (draw_area);

	load_scenario (fname);
	gtk_drawing_area_size (GTK_DRAWING_AREA (draw_area),
			       STONE_COLS  * STONE_SIZE,
			       STONE_LINES * STONE_SIZE);
	gtk_signal_connect (GTK_OBJECT(draw_area), "event", (GtkSignalFunc) area_event, 0);
}

static void
game_new_callback (GtkWidget *widget, void *data)
{
	new_game ();
}

static void
free_str (GtkWidget *widget, void *data)
{
	free (data);
}

static void
fill_menu (GtkWidget *menu)
{
	struct dirent *e;
	char *dname = gnome_unconditional_pixmap_file ("same-gnome");
	DIR *dir;
        int itemno = 0;
	
	dir = opendir (dname);

	if (!dir)
		return;
	
	while ((e = readdir (dir)) != NULL){
		GtkWidget *item;
		char *s = strdup (e->d_name);

		if (!strstr (e->d_name, ".png")) {
			free (s);
			continue;
		}
			
		item = gtk_menu_item_new_with_label (s);
		gtk_widget_show (item);
		gtk_menu_append (GTK_MENU(menu), item);
		gtk_signal_connect (GTK_OBJECT(item), "activate",
				    GTK_SIGNAL_FUNC (set_selection), s);
		gtk_signal_connect (GTK_OBJECT(item), "destroy",
				    GTK_SIGNAL_FUNC (free_str), s);
	  
	        if (!strcmp(scenario, s))
	        {
		  gtk_menu_set_active(GTK_MENU(menu), itemno);
		}
	  
	        itemno++;
	}
	closedir (dir);
}

static void
cancel (GtkWidget *widget, void *data)
{
	gtk_widget_destroy (pref_dialog);
	pref_dialog = 0;
}

static void
load_scenario_callback (GtkWidget *widget, void *data)
{
	if (selected_scenario.scenario) {
		load_scenario (selected_scenario.scenario);
		if (selected_scenario.make_it_default) {
			gnome_config_set_string (
				"/same-gnome/Preferences/Scenario", 
				selected_scenario.scenario);
			gnome_config_sync();
		}
	}
	cancel (0,0);
}

static void
game_preferences_callback (GtkWidget *widget, void *data)
{
	GtkWidget *menu, *omenu, *l, *hb, *cb, *f, *fv;
	GtkWidget *button;

	if (pref_dialog)
		return;
	
	pref_dialog = gnome_dialog_new (_("Preferences"),
			GNOME_STOCK_BUTTON_OK, GNOME_STOCK_BUTTON_CANCEL,
			NULL);
	gnome_dialog_set_parent (GNOME_DIALOG (pref_dialog), GTK_WINDOW (app));
	gtk_signal_connect (GTK_OBJECT(pref_dialog), "delete_event",
			    GTK_SIGNAL_FUNC (cancel), NULL);

	omenu = gtk_option_menu_new ();
	menu = gtk_menu_new ();
	fill_menu (menu);
	gtk_widget_show (omenu);
	gtk_option_menu_set_menu (GTK_OPTION_MENU(omenu), menu);

	f = gtk_frame_new (_ ("Scenario"));
	gtk_container_border_width (GTK_CONTAINER (f), 5);

	hb = gtk_hbox_new (FALSE, FALSE);
	gtk_widget_show (hb);
	
	l = gtk_label_new (_("Select scenario:"));
	gtk_widget_show (l);
	    
	gtk_box_pack_start_defaults (GTK_BOX(hb), l);
	gtk_box_pack_start_defaults (GTK_BOX(hb), omenu);

	cb = gtk_check_button_new_with_label ( _("Make it the default scenario") );
	gtk_signal_connect (GTK_OBJECT(cb), "clicked", (GtkSignalFunc)set_selection_def, NULL);
	gtk_widget_show (cb);

	fv = gtk_vbox_new (0, 5);
	gtk_container_border_width (GTK_CONTAINER (fv), 5);
	gtk_widget_show (fv);
	
	gtk_box_pack_start_defaults (GTK_BOX(fv), hb);
	gtk_box_pack_start_defaults (GTK_BOX(fv), cb);
	gtk_box_pack_start_defaults (GTK_BOX(GNOME_DIALOG(pref_dialog)->vbox), f);
	gtk_container_add (GTK_CONTAINER (f), fv);
	
	gtk_widget_show (f);
	
	gnome_dialog_button_connect (GNOME_DIALOG (pref_dialog), 0,
			GTK_SIGNAL_FUNC (load_scenario_callback), NULL);
	gnome_dialog_button_connect (GNOME_DIALOG (pref_dialog), 1,
			GTK_SIGNAL_FUNC (cancel), (gpointer)1);

        gtk_widget_show (pref_dialog);
}

static int
game_about_callback (GtkWidget *widget, void *data)
{
	GtkWidget *about;
	const gchar *authors[] = {
		"Miguel de Icaza.",
		"Federico Mena.",
		"Horacio J. Peña.",
		NULL
	};

	about = gnome_about_new (_("The Same Gnome"), VERSION,
				 "(C) 1997-1998 the Free Software Foundation",
				 (const char **)authors,
				 _("Original idea from KDE's same game program."),
				 /*"gnome-same-gnome.xpm"*/
				 NULL);
	gnome_dialog_set_parent(GNOME_DIALOG(about), GTK_WINDOW(app));
	gtk_widget_show (about);

	return TRUE;
}

static void
game_maybe_quit (GtkWidget *widget, int button)
{
	if (button == 0) {
		gtk_widget_destroy (app);
		gtk_main_quit ();
	}
}

static int
game_quit_callback (GtkWidget *widget, void *data)
{
	GtkWidget *box;
	
	box = gnome_message_box_new (_("Do you really want to quit?"),
				     GNOME_MESSAGE_BOX_QUESTION,
				     GNOME_STOCK_BUTTON_YES,
				     GNOME_STOCK_BUTTON_NO,
				     NULL);
	gnome_dialog_set_parent (GNOME_DIALOG(box), GTK_WINDOW(app));
	gnome_dialog_set_default (GNOME_DIALOG (box), 0);
	gtk_window_set_modal (GTK_WINDOW (box), TRUE);
	gtk_signal_connect (GTK_OBJECT (box), "clicked",
			   (GtkSignalFunc)game_maybe_quit, NULL);
	gtk_widget_show (box);

	return TRUE;
}

GnomeUIInfo gamemenu[] = {

        GNOMEUIINFO_MENU_NEW_GAME_ITEM(game_new_callback, NULL),

	GNOMEUIINFO_SEPARATOR,

	GNOMEUIINFO_MENU_SCORES_ITEM(game_top_ten_callback, NULL),

	GNOMEUIINFO_SEPARATOR,

        GNOMEUIINFO_MENU_EXIT_ITEM(game_quit_callback, NULL),

	GNOMEUIINFO_END
};

GnomeUIInfo settingsmenu[] = {
        GNOMEUIINFO_MENU_PREFERENCES_ITEM(game_preferences_callback, NULL),

	GNOMEUIINFO_END
};

GnomeUIInfo helpmenu[] = {
        GNOMEUIINFO_HELP("samegnome"),

	GNOMEUIINFO_MENU_ABOUT_ITEM(game_about_callback, NULL),

	GNOMEUIINFO_END
};

GnomeUIInfo mainmenu[] = {
	GNOMEUIINFO_MENU_GAME_TREE(gamemenu),
	GNOMEUIINFO_MENU_SETTINGS_TREE(settingsmenu),
	GNOMEUIINFO_MENU_HELP_TREE(helpmenu),
	GNOMEUIINFO_END
};

#define ELEMENTS(x) (sizeof (x) / sizeof (x [0]))


static int
save_state (GnomeClient *client,
	    gint phase,
	    GnomeSaveStyle save_style,
	    gint shutdown, 
	    GnomeInteractStyle interact_style,
	    gint fast,
	    gpointer client_data)
{
	gchar *prefix = gnome_client_get_config_prefix (client);
	gchar *argv []= { "rm", "-r", NULL };
	gchar *buf;
	struct ball *f = (struct ball*) field;
	int i;  
	
	if (debugging)
		g_print ("Saving state\n");
	
	gnome_config_push_prefix (prefix);
	
	gnome_config_set_int ("Game/Score", score);
	gnome_config_set_int ("Game/NStones", sync_stones ? 1 : nstones);
	
	buf= g_malloc (STONE_COLS*STONE_LINES+1);
	
	for (i = 0 ; i < (STONE_COLS*STONE_LINES); i++){
		buf [i]= f [i].color + 'a';
	}
	buf [STONE_COLS*STONE_LINES]= '\0';
	gnome_config_set_string ("Game/Field", buf);
	g_free(buf);
	
	gnome_config_pop_prefix ();
	gnome_config_sync();
	
	argv[2]= gnome_config_get_real_path (prefix);
	gnome_client_set_discard_command (client, 3, argv);
	
	return TRUE;
}


static void
restart (void)
{
	gchar *buf;
	struct ball *f = (struct ball*) field;
	int i;
	
	if (debugging)
		g_print ("Retrieving state\n");
	
	score = gnome_config_get_int_with_default ("Game/Score", 0);
	nstones = gnome_config_get_int_with_default ("Game/NStones", 0);
	
	buf = gnome_config_get_string_with_default ("Game/Field", NULL);  

	if (buf) {
		for (i= 0; i < (STONE_COLS*STONE_LINES); i++) 
		{
			f[i].color= buf[i] - 'a';
			f[i].tag  = 0;
			f[i].frame= nstones ? (rand () % nstones) : 0;
		}
		g_free (buf);
	}
}

static gint
client_die (GnomeClient *client, gpointer client_data)
{
        gtk_exit (0);

	return FALSE;
}

static const struct poptOption options[] = {
	{ NULL, 'd', POPT_ARG_NONE, &debugging, 0, N_("Debugging mode"), NULL },
	{ "scenario", 's', POPT_ARG_STRING, &fname, 0, N_("Set game scenario"), N_("NAME") },
	{ NULL, '\0', 0, NULL, 0 }
};

#ifndef GNOME_CLIENT_RESTARTED
#define GNOME_CLIENT_RESTARTED(client) \
(GNOME_CLIENT_CONNECTED (client) && \
 (gnome_client_get_previous_id (client) != NULL) && \
 (strcmp (gnome_client_get_id (client), \
  gnome_client_get_previous_id (client)) == 0))
#endif /* GNOME_CLIENT_RESTARTED */

int
main (int argc, char *argv [])
{
	GtkWidget *label;
	GnomeClient *client;

	gnome_score_init("same-gnome");

	bindtextdomain (PACKAGE, GNOMELOCALEDIR);
	textdomain (PACKAGE);

	gnome_init_with_popt_table ("same-gnome", VERSION, argc, argv, options, 0, NULL);

	gnome_window_icon_set_default_from_file (GNOME_ICONDIR"/gnome-gsame.png");
	client= gnome_master_client ();

	gtk_signal_connect (GTK_OBJECT (client), "save_yourself",
			    GTK_SIGNAL_FUNC (save_state), argv[0]);
	gtk_signal_connect (GTK_OBJECT (client), "die",
			    GTK_SIGNAL_FUNC (client_die), NULL);

	if (GNOME_CLIENT_RESTARTED (client)){
		gnome_config_push_prefix (gnome_client_get_config_prefix (client));
	    
		restart ();
		restarted = 1;
		
		gnome_config_pop_prefix ();
	}

	srand (time (NULL));

	app = gnome_app_new("same-gnome", _("Same Gnome"));

        gtk_window_set_policy(GTK_WINDOW(app), FALSE, FALSE, TRUE);
	gtk_signal_connect (GTK_OBJECT(app), "delete_event",
			    (GtkSignalFunc)game_quit_callback, NULL);

	appbar = gnome_appbar_new(FALSE, TRUE, GNOME_PREFERENCES_USER);
	gnome_app_set_statusbar(GNOME_APP (app), GTK_WIDGET(appbar));

	gnome_appbar_set_status(GNOME_APPBAR (appbar),
				_("Welcome to Same Gnome!"));

	gnome_app_create_menus(GNOME_APP(app), mainmenu);

	gnome_app_install_menu_hints(GNOME_APP (app), mainmenu);
  
        vb = gtk_vbox_new (FALSE, 0);
	gnome_app_set_contents (GNOME_APP (app), vb);

	if (!fname) {
		fname = gnome_config_get_string
			("/same-gnome/Preferences/Scenario=stones.png");
	}

	create_same_board (fname);

	label = gtk_label_new (_("Score: "));
	scorew = gtk_label_new ("");
	set_score (score);

	gtk_box_pack_start(GTK_BOX(appbar), label, FALSE, TRUE, 0);
	gtk_box_pack_start(GTK_BOX(appbar), scorew, FALSE, TRUE, 0);
	
	if (!restarted)
		new_game ();
	
	g_free (fname);

	gtk_widget_show (vb);
	gtk_widget_show (GTK_WIDGET(label));
	gtk_widget_show (GTK_WIDGET(scorew));
        gtk_widget_show (app);

	gtk_main ();
	return 0;
}
