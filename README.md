THIS IS A BLOG SOFTWARE
IT HAS POST TYPES


TEMPLATE/ IS FAKE AND WAS FOR MY OWN REFERENCE

POSTS/ STORES SOME POSTS---THESE ARE ALSO FAKE

TO GENERATE POSTS, LOAD GENERATE-POSTS.SCM AND CALL
(generate-posts)

IT IS FUN LIKE THAT.

POST TYPES ARE PRETTY EASY TO ADD.

Okay, I added tagging.

The .post files contain a (tags . (...))

You just add .post files to posts/

The four structures are there.

And then you load generate-posts.scm and call (generate-posts)

And it just churns out tons of HTML

The preamble and postamble defined in generate-posts.scm determine how the pages look

(Well, that and index.css)

So the first ~70 linkes of generate-posts.scm contain all of the formatting stuff, and the rest of it is more or less in there, too.

Individual post types are [type]-post.scm and those should be fairly easy to modify/whatever for formatting.

The entirety of the code is ~675 lines, so nothing should be too hard to find.


