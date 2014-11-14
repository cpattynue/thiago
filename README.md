thiago - the THIAGO erlang application
======

Compile the application 
	$ make all

Before start application, you must configure the path to save the files in  ebin/thiago.app at line 11.
	
	11 {env, [{ path, "" }]},

Example 
	11 {env, [{ path, "/Users/Documents/folder" }]},

Start the application with the code loaded
	
	$ erl -pa ebin/

	1> application:start(thiago).
	ok

Create a file, you need a name of the file and a list of the data to save 
	
	2> thiago:create({"name", [{users, "0001"}]}).  
	ok

Get data, you need name of the file and the name of the bucket

	3> thiago:get({"name", users}).
	[{users,0001}]
	ok

Update file, you need name of the file and the data to add

	4> thiago:update({"name", [{users , "0002" }]}).
	ok

Delete one row, you need the name of the file and the data of the row to delete

	5> thiago:delete({"name", {users , "0002" }}).  
	ok 

Delete all data in the bucket, you need atom all and the name of the file 

	6> thiago:delete({all, "name"}).
	ok

Stop application
	
	7> application:stop(thiago).

===
