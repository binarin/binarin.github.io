all: ./themes/simple/static/css/styles.css
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
