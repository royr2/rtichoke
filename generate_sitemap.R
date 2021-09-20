sitemap <- ""

for(f in list.files("content/post")){
  sitemap <- paste0(sitemap, paste0("https://rtichoke.netlify.app/post/", f, ".html"), "\n")
}

writeLines(sitemap, "public/sitemap.txt", useBytes=T)
