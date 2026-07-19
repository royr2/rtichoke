# R'tichoke

A personal blog about R, analytics, risk, and random ideas. Built with [Quarto](https://quarto.org/) and deployed to [rtichoke.netlify.app](https://rtichoke.netlify.app).

## Repository structure

```
.
├── _quarto.yml          # site config (navbar, theme, format)
├── _publish.yml         # Netlify publish target
├── index.qmd            # home page
├── about.qmd            # about page
├── installation.qmd     # R / RStudio install guide
├── webr.qmd             # WebR (in-browser R) landing page
├── webr-playground.html # hand-rolled WebR REPL (standalone HTML)
├── posts.qmd            # blog listing
├── posts/               # long-form posts (one .qmd per post)
├── get-started/         # beginner R tutorials
├── images/              # post thumbnails and figures
├── styles.css           # custom theme overrides
├── install_packages.R   # script that installs all R deps via pak
└── rtichoke.Rproj       # RStudio project file
```

## Build locally

You need [Quarto](https://quarto.org/docs/get-started/) and R ≥ 4.5 installed.

```bash
# 1. Install R dependencies used across the posts
Rscript install_packages.R

# 2. Render and preview the site
quarto preview
```

`quarto preview` watches for changes and serves at `http://localhost:4200` (or a nearby port). `quarto render` produces the static site in `_site/`.

## Add a post

1. Create `posts/<my-post-slug>.qmd`.
2. Frontmatter (the listing reads `title`, `date`, `categories`, `image`):

   ```yaml
   ---
   title: "My Post Title"
   date: "YYYY-MM-DD"
   categories: [R, Topic]
   image: "../images/my-post.png"
   execute:
     echo: true
     warning: false
     message: false
     eval: true
     cache: true   # recommended for posts that hit the network or run ML
   ---
   ```

3. Add any thumbnail/figure to `images/`.
4. If you used a package not already in `install_packages.R`, add it there so CI/Netlify builds don't fail.

## Conventions

- Filenames: kebab-case (e.g. `bayesian-optimization-xgboost.qmd`). Some older posts use snake_case; they'll be migrated over time.
- Stochastic posts: call `set.seed()` before any `runif`/`sample`/optimization so renders are reproducible.
- Network-dependent posts (e.g. `getSymbols`, remote CSVs): set `execute.cache: true` and pin external URLs to a commit SHA where possible.

## Deploy

Configured in `_publish.yml` to push to Netlify (site id `4c4422d2-8fa2-40ce-a413-85690dad6e3f`, target `rtichoke.netlify.app`). Either `quarto publish` or Netlify's Git-connected auto-build from the `main` branch.

## Reproducibility

R dependencies are not yet pinned with `renv`. To snapshot the current environment:

```bash
Rscript -e 'renv::init()'
```

This creates `renv.lock` and `renv/activate.R`; commit `renv.lock` so future builds use the same package versions. (`renv` is already in `install_packages.R`.)

## License

Code in this repository is licensed under the [MIT License](./LICENSE). Written content (blog posts in `posts/*.qmd`, `get-started/*.qmd`) is © Riddhiman Roy — see the LICENSE file for details.
