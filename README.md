cd hakyll
stack exec -- site build && cp -r _site/* ../webroot/
cd ../webroot && scp -r posts/index.html chrisdone:/var/www/html/posts/
