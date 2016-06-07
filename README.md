# Parenthepress

This is a static blogging engine using Chez Scheme and Perl.

It's novel over most static blogging engings in that it uses `post types` (a la Tumblr).

## Usage

To generate posts, load `generate-posts.scm` and run

    > (generate-posts)

And it just churns out tons of HTML



### Adding new posts

Posts are structured as s-exprs. To add a post, call it `*.post` and add it to `posts/`. Look at the existing templates.

## Structure

- `posts/` stores some posts
- `img/` stores blog images


The preamble and postamble defined in `generate-posts.scm` determine how the pages look (along with `index.css`).

The first ~70 linkes of `generate-posts.scm` contain all of the formatting stuff, and the rest of it is more or less in there, too.

Individual post types are `[type]-post.scm` and those should be fairly easy to modify/whatever for formatting.

(The entirety of the code is ~675 lines, so nothing should be too hard to find.)


