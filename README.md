thiago - :smiley_cat: - write erlang terms to file 
======

Thiago is the name of my CAT :smiley_cat: :smiley_cat: :smiley_cat: :smiley_cat:, 
but also is an application written in erlang to store erlang terms into files, 
using disk erlang term storage

How to start?
======

Clone from github:

	$ git clone https://github.com/cpattynue/thiago.git

Compile the application: 
	
	$ make all

Before start application, you must configure the path to save the files in the environment of
the app:
	
	{env, [{ path, "" }]},

Start the application with the code loaded
	
	$ erl -pa ebin/

	1> application:start(thiago).
	ok

Create a file, you need a name of the file and a list of the data to save 
	
	2> thiago:create({"name", [{users, "0001"}]}).  
	ok

Get data, you need name of the file and the name of the bucket

	3> thiago:get({"name", users}).
	{ok, [{users,0001}]}

Update file, you need name of the file and the data to append 

	4> thiago:update({"name", [{users , "0002" }]}).
	ok

Delete one row, you need the name of the file and the data of the row to delete

	5> thiago:delete({"name", {users , "0002" }}).  
	ok 

Delete all data in the bucket, you need atom all and the name of the file 

	6> thiago:delete({"name", all}).
	ok

