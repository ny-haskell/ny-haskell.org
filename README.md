# ny-haskell.org

This site is created with jekyll, a static site generator. One day we might have a haskell version of the site, but until then here are the instructions for contributing to the repo!


## Installation

1. Jekyll (and ruby)
2. Clone this repo
3. Submit contributions with pull requests

Github pages explains how to install jekyll to run locally on your machine. You will need ruby to install jekyll.

[https://help.github.com/articles/using-jekyll-as-a-static-site-generator-with-github-pages/](https://help.github.com/articles/using-jekyll-as-a-static-site-generator-with-github-pages/)

## Contributing

You can check the [issue tracker](https://github.com/ny-haskell/ny-haskell.org/issues) for ideas, or you can write up a blog post and submit it with a pull request. 

### Site Structure

```
├── _data
├── _includes
├── _layouts
├── _posts
├── assets
│   ├── css
│   ├── fonts
│   ├── img
│   └── js
└── slack
```

### Video Links

Video links are organized under the `_data/videos.json` file. We don't host the videos here, but rather embed youtube videos so the json file contains video IDs as well as other metadata.

### Blog posts

When you write blog posts, include the file under the `_posts` directory. Some things you can write about include: NY-Haskell events you've attended, your favorite haskell libraries, tutorials on how to use libraries, or even installation and setup tips. 

