y := minc_grammar.y
py := $(patsubst %.y,%.py,$(y))
cs := $(wildcard test/f0.c)
#cs := test/fun.c
xmls := $(patsubst %.c,%.xml,$(cs))

all : $(xmls) $(py)

$(py) : %.py : %.y
	tatsu $< > $@

$(xmls) : %.xml : %.c $(py) minc_to_xml.py
	python3 minc_to_xml.py $< > $@

.DELETE_ON_ERROR:

