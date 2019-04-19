# Raw Assets

This index.html file replaces the GHCJS generated index.html file.

As per the documentation, GHCJS does not overwite the file if it already exists.

Customizations to index.html are allowed an acceptable.

Since we do not want to include builds in source control and the GJHCJS
generated index.html resides inside of "dist" we will use this index.html
to replace the generated one when "dist" is copied over for use by the server.

This is necessary to manage any special JS dependencies, in our case for
Foundation.
