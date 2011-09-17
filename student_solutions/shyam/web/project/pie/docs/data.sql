<html>
<head></head>
<body bgcolor=white>
<pre>

-- Data model for PARTNERS IN EDUCATION project

-- 1. Data model for users and user groups
-- Used for registration and authentication. Also maintains session infomation implemented via cookies
-- Everyone has to be registered to see any part of the site.

create sequence user_seq start with 1;

create table users (          
        user_id                 integer
				constraint users_id_pk
				primary key,
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

create table sessions (
       user_id			not null references users,
       session_id		integer not null,
       expiration_time		integer not null
);


-- 2. Data model for contents
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

-- now create an Intermedia index, which does stemming, on the content clob of the discussions table.
-- Note that we do not have just one column containing all of the searchable text.
-- Note that this index must be rebuilt, perhaps nightly.

create index discussion_idx on discussions(content) indextype is ctxsys.context;



-- 3. Data model for assessments
-- This is a separate module that is used for collecting patient-related information

-- Patient identifying information
create sequence names_seq start with 1;
create table names (
       id			integer not null primary key,
       mgh_id			integer,
       first_names		varchar(100) not null,
       last_name		varchar(100) not null,
       birth_date		date,
       creation_date		date default sysdate not null
);

-- Gail risk assessment for breast cancer for menopausal women
-- considering hormonal replacement
create sequence gail_seq start with 1;
create table gail (
       gail_id			integer not null primary key,
       id			references names,
       race			varchar(10) check (race in ('white', 'black', 'asian')),
       age_at_menarche		integer,
       age_at_first_live_birth	integer,
       relatives_with_cancer	integer,
       biopsy_count		integer,
       biopsy_hyperplasia	varchar(10) check (biopsy_hyperplasia in ('yes', 'no', 'unknown')),
       creation_date		date not null,
       edit_date		date not null
);

-- Heart risk assessment for menopausal women considering
-- hormonal replacement
create sequence heart_seq start with 1;
create table heart (
       heart_id			integer not null primary key,
       id			references names,
       total_cholesterol	integer,
       hdl_cholesterol		integer,
       systolic_bp		integer,
       diabetes			varchar(10) check (diabetes in ('yes', 'no')),
       smoker			varchar(10) check (smoker in ('yes', 'no')),
       creation_date		date not null,
       edit_date		date not null
);

</pre>
</body>
</html>

