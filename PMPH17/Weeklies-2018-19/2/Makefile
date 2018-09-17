# If not installed, try 'sudo apt-get install asciidoctor source-highlight' on
# Debian-based systems.

.PHONY: all clean


all: assignment2.html

assignment2.html: assignment2.asciidoc
	asciidoctor -a data-uri -b html5 assignment2.asciidoc

clean:
	rm -f assignment2.html solution.html
