all: ./themes/simple/static/css/styles.css
	hugo

./themes/simple/static/css/styles.css: ./themes/simple/static/css/styles.less
	lessc $< $@
