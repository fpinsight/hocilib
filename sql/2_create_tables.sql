create type type_t as OBJECT (id int, name varchar2(50));
/

create type test_t as object (
       val_int  number,
       val_flt  float,
       val_dbl  float,
       val_str  varchar2(30),
       val_date date,
       val_lob  clob,
       val_file bfile,
       val_obj  type_t,
       val_raw  raw(10)
);
/

create type t_tab1_emp as VARRAY(100) of varchar2(50);
/

create type t_tab2_emp as table of varchar2(50);
/

create table test_fetch(code int, article varchar2(30), price float, creation date);

create table test_long_raw(code int, content long raw);

create table test_long_str(code int, content long);

create table test_lob(code int, content clob);

create table test_object(val test_t);

create table test_table_obj of type_t;

create table test_array (
       val_int  number,
       val_dbl  float,
       val_flt  float,
       val_str  varchar2(30),
       val_date date,
       val_lob  clob,
       val_file bfile
);

create table test_coll_varray (
       departement number,
       employees   t_tab1_emp
);

create table test_coll_nested
(
       departement number,
       employees t_tab2_emp
) nested table employees store as test_table_emp;

create table test_directpath(
       val_int number(8,4),
       val_str varchar2(30),
       val_date date
);

/* insert data into the demo tables */

insert into test_fetch (code, article, price, creation) values (1, 'shoes', 3.14, to_date('1978-12-23', 'YYYY-MM-DD'));
insert into test_fetch (code, article, price, creation) values (2, 'shirt', 5.99, to_date('1999-09-12', 'YYYY-MM-DD'));

insert into test_lob(code,content) values (1, EMPTY_CLOB());

insert into test_long_str(code,content) values (1, 'Rugby rocks !');

insert into test_coll_varray(departement,employees) values (1, t_tab1_emp('Peter', 'John', 'Paula', 'Gina'));
insert into test_coll_varray(departement,employees) values (2, t_tab1_emp('Ben', 'Alice', 'Joel', 'Maria'));

insert into test_coll_nested(departement,employees) values (1, t_tab2_emp('Vince', 'Richard', 'Rita', 'Sophia'));
insert into test_coll_nested(departement,employees) values (2, t_tab2_emp('Paul', 'Sarah', 'Robert', 'Zoe'));

insert into test_table_obj values(type_t(1, 'shoes'));
insert into test_table_obj values(type_t(2, 'pen'));
