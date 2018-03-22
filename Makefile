all: ./themes/simple/static/css/styles.css images
	hugo

.ONESHELL:

commit: all
	git worktree prune
	if ! git worktree list | grep -q .git/publish-wt; then \
	    git worktree add .git/publish-wt master;            \
	fi
	cd .git/publish-wt
	git ls-files | xargs rm -f

	rsync -va ../../public/ . | sed '0,/^$$/d'

	git add .
	git commit --allow-empty -m "Updated on $$(date -R) from $$(git -C ../../ rev-parse --short HEAD)"

push: commit
	cd .git/publish-wt && git push

./themes/simple/static/css/styles.css: ./themes/simple/static/css/styles.less ./themes/simple/static/css/*.less
	lessc $< $@

SRC_IMAGES = $(wildcard ../org/blog-pics/*.jpg)
DST_IMAGES = \
	$(patsubst ../org/blog-pics/%.jpg,./static/images/responsive/%.jpg,$(SRC_IMAGES)) \
	$(patsubst ../org/blog-pics/%.jpg,./static/images/responsive/%-320.jpg,$(SRC_IMAGES)) \
	$(patsubst ../org/blog-pics/%.jpg,./static/images/responsive/%-480.jpg,$(SRC_IMAGES)) \
	$(patsubst ../org/blog-pics/%.jpg,./static/images/responsive/%-800.jpg,$(SRC_IMAGES)) \

images: $(DST_IMAGES)

./static/images/responsive/%-320.jpg: ../org/blog-pics/%.jpg
	convert $< -resize 320 $@

./static/images/responsive/%-480.jpg: ../org/blog-pics/%.jpg
	convert $< -resize 480 $@

./static/images/responsive/%-800.jpg: ../org/blog-pics/%.jpg
	convert $< -resize 800 $@

./static/images/responsive/%.jpg: ../org/blog-pics/%.jpg
	cp $< $@
