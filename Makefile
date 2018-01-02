.PHONY: clean

clean:
	find . -name .DS_Store -print0 | xargs -0 rm -rf
	find . -name .Rhistory -print0 | xargs -0 rm -rf
	find . -type d -name *_files -print0 | xargs -0 rm -rf
	find . -type d -name *_cache -print0 | xargs -0 rm -rf
