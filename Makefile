default: arvo
clean :
	rm -rf arvo-img*

arvo: clean
	ml-build arvo.cm Main.main arvo-img

.PHONY : default clean arvo
