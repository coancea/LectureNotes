# If not installed, try 'sudo apt-get install asciidoctor source-highlight' on
# Debian-based systems.

.PHONY: all clean


all: assignment3.html

assignment3.html: assignment3.asciidoc
	asciidoctor -a data-uri -b html5 assignment3.asciidoc

clean:
	rm -f assignment3.html
