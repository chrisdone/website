```
cd hakyll

docker run -it --net=host -v`pwd`:`pwd` -w`pwd` --rm chrisdone/website-hakyll2 site build

cp -r _site/* ../webroot/

cd ../webroot && scp -r posts/index.html chrisdone:/var/www/html/posts/
```

Anything.
