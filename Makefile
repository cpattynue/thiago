
SRC=$(PWD)/src/
EBIN=$(PWD)/ebin/

all: compile 

compile: 
	erlc -o $(EBIN)  $(SRC)*.erl

clean:
	@rm -rf $(EBIN)*.beam
