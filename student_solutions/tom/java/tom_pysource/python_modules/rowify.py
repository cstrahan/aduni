####################################################################################################################
### rowify.py
### Bryon Gill 4/24/01
### This module contains html row formatting functions for the secondary
### sources web page.
import Ns
        ###TODO FOR ROWIFY:
        ### -colorize the rows, alternating for easy reading- build this into the rowify function?
        ### -make the team names into links to team pages
        ### -test the dates, and print the ones that are less than 7 days old in red
def rowify(my_db_handle, my_query_string):
     caselist = ""
     try:
             my_results = my_db_handle.Select(my_query_string)
             color_determinant = ''

             while (my_db_handle.GetRow(my_results) == Ns.OK):
                  team_id = my_results['team_id']
                  
                  ################# added by Blake Asdorian, 4/24/01 to allow comments about particular caselist items
                  list_id = my_results['list_id']
                  #########################################
                  
                  try:  ###This awful hack saved me much time.  Either a team name will be displayed, or the author of the comment.
                       team_or_author = "<li>" + my_results['school_name'] + my_results['team_name']  + "<br>posted by " + my_results['email']
                       school = ""

                  except:
                       school = my_results['school_name']
                       team_or_author = my_results['team_name']
                  title = my_results['title']
                  date = my_results['date_posted'][5:10]
                  if color_determinant == 'BGCOLOR="pink"':
                       color_determinant = 'BGCOLOR=""'
                  else:
                       color_determinant = 'BGCOLOR="pink"'
                  ###if there's a message passed in, display it.
                  try:
                       message = "<P>" + my_results['message'] + "<p> <a href=/project/submit/content_submission.py?team_id=" + team_id + "&list_id=" + list_id + ">submit a comment</a>"
                  except:
                       message = ""
                  caselist = caselist + '''
                  <TR %s>
                  <TD align=center><a href=/project/teampage.py?team_id=%s>%s %s</TD>
                  <TD align=left><li>%s %s</TD>
                  <TD align=center>%s</TD>
                  </TR>''' % (color_determinant,team_id,school,team_or_author,title,message,date)
     except:
          caselist = "<tr><td>You'll have to try again when we can afford more bandwidth.</td></tr>"
     return caselist




















