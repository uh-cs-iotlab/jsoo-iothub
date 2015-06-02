all: plugins

plugins: google-calendar
	
google-calendar:
	 cd plugins/google-calendar; make

