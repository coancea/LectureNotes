# If not installed, try 'sudo apt-get install asciidoctor source-highlight' on
# Debian-based systems.

.PHONY: all clean


all: assignment1.html solution.html

assignment1.html: assignment1.asciidoc
	asciidoctor -a data-uri -b html5 assignment1.asciidoc

solution.html: solution.md
	pandoc -s --smart --email-obfuscation=none -t html5 solution.md > solution.html

clean:
	rm -f assignment1.html solution.html
