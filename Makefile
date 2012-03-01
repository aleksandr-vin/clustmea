
.SUFFIXES:	.erl .beam .yrl

.erl.beam:
	erlc -W $<

MODS =	clustmea clustmea_sup \
	clustmea_conf clustmea_task \
	misc graphviz

all: compile

compile:	${MODS:%=%.beam}

clean:
	rm -vrf ${MODS:%=%.beam} erl_crash.dump
