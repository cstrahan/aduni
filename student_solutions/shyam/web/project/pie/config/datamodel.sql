SET SCAN OFF
-- this allow @ in an email address

-- Data model for users and user groups
-- Used for registration and authetication. Also  maintains session infomation implemented via cookies

-- Everyone has to be registered to see any part of the site.

drop index discussion_idx;
drop index article_idx;

drop table discussions cascade constraints;
drop table articles cascade constraints;
drop sequence content_seq;

drop table sessions cascade constraints;
drop table user_group_map cascade constraints;
drop sequence group_seq;
drop table groups cascade constraints;
drop sequence user_seq;
drop table users cascade constraints; 

create sequence user_seq start with 1;

create table users (          
        user_id                 integer
				constraint users_id_pk
				primary key,
	 -- This is for the "Dr." prefix
        name_prefix             varchar(20),     
        -- This is for the users first names (or first and middle name)
        -- example Jhon Doe Smith would have a first_names of "Fhon Doe"
        -- and a last_name of "Smith"
        first_names             varchar(100)
				constraint users_first_names_nn
				not null,
        last_name               varchar(100)
				constraint users_last_name_nn
				not null,
        name_suffix             varchar(20),
	-- email is unique, however real emails are not case sensitive
        email                   varchar(100)
				constraint users_email_nn				
				not null
				unique,
        email_bouncing_p        char(1) default 'f' 
				constraint users_email_bouncing_p_ck
				check (email_bouncing_p in ('t','f')),
        password                varchar(30)
				constraint users_password_nn				
				not null,
        phone_home		varchar(30),
        phone_work		varchar(30),
        last_visit              date,
        second_to_last_visit    date,
        registration_date       date default sysdate
				constraint users_registration_date_nn
				not null,
        creation_user           not null references users,
        status                  varchar(10) default 'active'
				constraint user_status_ck
				check (status in ('active','inactive','banned')),
        banning_user            references users,
        banning_date            date,
        banning_note            varchar(4000)
);

insert into users (user_id, name_prefix, first_names, last_name, email, password, last_visit, registration_date, creation_user, status)
       values (user_seq.nextval, 'Ms', 'Guneet', 'Kaur', 'guneet@test.com', 'test', sysdate, sysdate, 1, 'active');

insert into users (user_id, name_prefix, first_names, last_name, email, password, last_visit, registration_date, creation_user, status)
       values (user_seq.nextval, 'Dr', 'Susan', 'Oliverio', 'susan@test.com', 'test', sysdate, sysdate, 1, 'active');

insert into users (user_id, name_prefix, first_names, last_name, email, password, last_visit, registration_date, creation_user, status)
       values (user_seq.nextval, 'Ms', 'Jennifer', 'Smith', 'jennifer@test.com', 'test', sysdate, sysdate, 1, 'active');


--               users.phone_work,
update users set users.phone_home= '1-617-738-9721'       where user_id = 2;
update users set users.last_visit       = sysdate                where user_id = 2;
--               users.second_to_last_visit

create sequence group_seq start with 1;

create table groups (
        group_id                integer
				constraint group_id_pk
				primary key,
        short_name		varchar(100)
				constraint groups_short_name_nn
				not null
				check (short_name in ('admin', 'practice', 'meeting')),
	pretty_name		varchar(200),
        description             varchar(4000),
        status                  varchar(10) not null check (status in ('active','inactive')),
	meeting_time		varchar(100),
	meeting_place		varchar(200),
	meeting_note		varchar(4000),
        creation_user		constraint groups_creation_users_nn
				not null			
				constraint groups_creation_users_fk
				references users,
        creation_date           date default sysdate
				constraint groups_creation_date_nn
				not null,
	-- programmatically reset when status inactive->active
        inactivation_user       constraint groups_inactivation_user_fk
				references users,
	-- programmatically reset when status inactive->active
        inactivation_date       date
);


insert into groups (group_id, short_name, pretty_name, description, status, creation_user, creation_date)
       values (group_seq.nextval, 'admin', 'Administration', 'This person is an administrator of this site', 'active', 1, sysdate);
insert into groups (group_id, short_name, pretty_name, description, status, creation_user, creation_date)
       values (group_seq.nextval, 'practice', 'Doc Area', 'This person is a doctor with a practice', 'active', 1, sysdate);


create table user_group_map (
        user_id                 not null
				constraint user_group_map_user_id_fk
				references users,
        group_id                not null
				constraint user_group_map_group_id_fk
				references groups,
        role                    varchar(20) check (role in ('leader', 'member')),
        creation_date           date default sysdate
				constraint ugmap_date_nn
				not null,
        -- e.g. unique on pair
	constraint user_group_map_unique_un
        unique(user_id, group_id)
);

insert into user_group_map (user_id, group_id, role, creation_date)
       values (1, 1, NULL,  sysdate);

insert into user_group_map (user_id, group_id, role, creation_date)
       values (2, 2, 'leader',  sysdate);

insert into user_group_map (user_id, group_id, role, creation_date)
       values (3, 2, 'member',  sysdate);

create table sessions (
       user_id			not null references users,
       session_id		integer not null,
       expiration_time		integer not null
);

-- Types of content:
--	 1. Expert article
--	 2. Response to expert article
--	 3. Question on a menopause-discussion system
--	 4. Response to above question

-- Functionality (from user's / patient's perspective)
--		- Post response to article / question
--		- Post new question
--		- search content system

-- Funtionality (from moderator/admin perspective)
--		- edit/delete any content

create sequence content_seq start with 1;

create table articles (
       content_id	integer primary key,
       title		varchar(300),
       category		varchar(200),	
       content		clob,
       parent_id	constraint articles_parent_id_fk
			references articles(content_id)
			on delete cascade,
       posting_user	not null references users,
       posting_time	date default sysdate not null
);

insert into articles (content_id, title, category, content, posting_user, posting_time)
       values (content_seq.nextval, 'An Expert Test Article', 'Expert', 'Estrogen stuff...', 2, sysdate);
insert into articles (content_id, title, category, content, posting_user, posting_time)
       values (content_seq.nextval, 'Another Expert Test Article', 'Another', 'Hot and cold flashes...', 2, sysdate);
insert into articles (content_id, title, content, posting_user, posting_time)
       values (content_seq.nextval, 'Yet Another Expert Test Article', 'A primer on menopause...', 2, sysdate);


-- now create an Intermedia index, which does stemming, on the content clob of the articles table.
-- Note that we do not have just one column containing all of the searchable text.
-- Note that this index must be rebuilt, perhaps nightly.
create index article_idx on articles(content) indextype is ctxsys.context;

create table discussions (
       content_id	integer primary key,
       title		varchar(300),
       category		varchar(200),	
       content		clob,
       parent_id	constraint discussions_parent_id_fk
			references discussions(content_id)
			on delete cascade,
       posting_user	not null references users,
       posting_time	date default sysdate not null
);

insert into discussions (content_id,  title, category, content, posting_user, posting_time)
       values (content_seq.nextval, 'Menopause question', 'Menopause', 'This is a menopause question...', 3, sysdate);

insert into discussions (content_id, category, content, parent_id, posting_user, posting_time)
       values (content_seq.nextval, 'Menopause', 'Replying to menopause question', 4, 2, sysdate);

-- now create an Intermedia index, which does stemming, on the content clob of the discussions table.
-- Note that we do not have just one column containing all of the searchable text.
-- Note that this index must be rebuilt, perhaps nightly.
create index discussion_idx on discussions(content) indextype is ctxsys.context;

commit;








