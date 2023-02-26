# rezonateR

[`rezonateR`](rezonators.github.io/rezonateR/) (pronounced "resonate R") uses R (statistics software) to analyze and visualize data about natural language discourse. The data that `rezonateR` takes as input are those created by the companion tool [Rezonator](https://github.com/johnwdubois/rezonator). While the Rezonator tool is used for manual annotating and visualising discourse features and useful for qualitative analysis, `rezonateR` takes Rezonator input and helps perform automated or semi-automated annotation and prepare the data for quantitative analysis in R.

Some examples of research questions you could investigate with Rezonator + `rezonateR` include:

-   In spoken English discourse, what factors can be used to predict when a speaker will use a pronoun instead of a noun phrase to refer to some entity?
-   Do speakers tend to repeat syntactic structures just used by their interlocutor if they agree with them, or if they disagree with them?
-   What factors affect the ordering of noun-phrase arguments within the clause in Tibetan, an overwhelmingly verb-final language with flexible word order among arguments?
-   In English, the form *that* is often said to perform different functions, such as a complementiser, demonstrative determiner, demonstrative pronoun, etc. What are the discourse profiles of these different uses, and is there any way they are similar?

If you're starting out, this site contains different resources that you can use to learn how to use the tool, including:

-   **Toy example**: If you aren't very familiar with the companion tool Rezonator and want to know what can be done with this tool, the [toy example](https://rezonators.github.io/rezonateR/articles/sample_proj.html) `vignette("sample_proj")` gives a concrete example of a mini-research project using Rezonator and `rezonateR`, and will give you a feel of what sorts of projects are possible with Rezonator + `rezonateR`. It would also be helpful to check out the [official Rezonator guides](https://rezonator.com/guide/) and try some simple annotations in Rezonator to get a feel of how it works.
-   **Overview for Rezonator users**: If you are already familiar with how Rezonator works but want to know how rezonateR can extend and enhance your work in Rezonator, the [tutorial](https://rezonators.github.io/rezonateR/articles/import_save_basics.html) `vignette("overview")` is a bird's eye view of the most important functionality of the package.
-   **Detailed walkthrough**: If you are already confident that `rezonateR` is the tool you want to be using, the series of tutorials starting from the one on [imports, saves, and data structure](https://rezonators.github.io/rezonateR/articles/import_save_basics.html) `vignette("import_save_basics")` goes into the nitty-gritty of how to use `rezonateR`. This doubles as a reference to look up how to do different things in `rezonateR`.
-   **Documentation**: If you need more detailed explanation of how individual functions, you can access the documentation of `rezonateR` with the [reference tab](https://rezonators.github.io/rezonateR/reference/index.html) if you're on the website, or with `help(package = "rezonateR")` if you're on your local machine.

This is the first official release of `rezonateR`. Currently, it contains a rich set of functions for editing and analysing coreference-related phenomena. Functionality for visualisation and analysing resonance and turn-taking phenomena to be added soon. Functionaity is added on a needs-based basis, so please [submit an issue on GitHub](https://github.com/rezonators/rezonateR/issues) if you need any!

For the public-facing website for the entire project, see [Rezonator.com](https://rezonator.com/).

# Acknowledgements

Thanks to Giorgia Troiani, Stefan Th Gries, Argyro Katsika and the UCSB CEILing group for their comments on this project.
