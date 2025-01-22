<!-- omit in toc -->
# Contributing to LightLogR

First off, thanks for taking the time to contribute! ❤️

All types of contributions are encouraged and valued. See the [Table of Contents](#table-of-contents) for different ways to help and details about how this project handles them. Please make sure to read the relevant section before making your contribution. It will make it a lot easier for us maintainers and smooth out the experience for all involved. The community looks forward to your contributions. 🎉

> And if you like the project, but just don't have time to contribute, that's fine. There are other easy ways to support the project and show your appreciation, which we would also be very happy about:
> - Star the project
> - Tweet about it
> - Refer this project in your project's readme or publications that make use of the software
> - Mention the project at local meetups and tell your friends/colleagues

<!-- omit in toc -->
## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [I Have a Question](#i-have-a-question)
- [Why is my light logger, dosimeter, or other wearable device not supported by LightLogR](#i-want-my-logger)
- [I want News about LightLogR](#i-want-news)
- [I Want To Contribute](#i-want-to-contribute)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Your First Code Contribution](#your-first-code-contribution)
- [Improving The Documentation](#improving-the-documentation)
- [Styleguides](#styleguides)
- [Commit Messages](#commit-messages)
- [Join The Project Team](#join-the-project-team)


## Code of Conduct

This project and everyone participating in it is governed by the
[LightLogR Code of Conduct](https://tscnlab.github.io/LightLogR/CODE_OF_CONDUCT.html).
By participating, you are expected to uphold this code. Please report unacceptable behavior
to the [lead developer](mailto:johannes.zauner@tum.de).


## I Have a Question

> If you want to ask a question, we assume that you have read the available [Documentation](https://tscnlab.github.io/LightLogR/).

Before you ask a question, it is best to search for existing [Issues](https://github.com/tscnlab/LightLogR/issues) that might help you. In case you have found a suitable issue and still need clarification, you can write your question in this issue. It is also advisable to search the internet for answers first.

If you then still feel the need to ask a question and need clarification, we recommend the following:

- Open an [Issue](https://github.com/tscnlab/LightLogR/issues/new).
- Provide as much context as you can about what you're running into.
- Provide project and platform versions, depending on what seems relevant.

We will then take care of the issue as soon as possible.

## Why is my light logger, dosimeter, or other wearable device not supported by LightLogR

If you are using a device that is currently not supported, please contact us. 
You can find a list of supported devices in the [LightLogR documentation about imports](https://tscnlab.github.io/LightLogR/reference/import_Dataset.html#devices).

We are always looking to expand the range of supported devices. The easiest and most trackable way to get in contact is by opening a new issue on our [Github repository](https://github.com/tscnlab/LightLogR/issues). Please also provide a sample file of your data, so we can test the import function.

## I want News about LightLogR

If you are interested in the project and want to know more, you can subscribe to the [LightLogR mailing list](https://lists.lrz.de/mailman/listinfo/lightlogr-users)

## I Want To Contribute

<!-- omit in toc -->
### Legal Notice 

> When contributing to this project, you must agree that you have authored 100% of the content, that you have the necessary rights to the content and that the content you contribute may be provided under the project licence.

### Reporting Bugs

<!-- omit in toc -->
#### Before Submitting a Bug Report

A good bug report shouldn't leave others needing to chase you up for more information. Therefore, we ask you to investigate carefully, collect information and describe the issue in detail in your report. Please complete the following steps in advance to help us fix any potential bug as fast as possible.

- Make sure that you are using the latest version.
- Determine if your bug is really a bug and not an error on your side e.g. using incompatible environment components/versions (Make sure that you have read the [documentation](https://tscnlab.github.io/LightLogR/). If you are looking for support, you might want to check [this section](#i-have-a-question)).
- To see if other users have experienced (and potentially already solved) the same issue you are having, check if there is not already a bug report existing for your bug or error in the [bug tracker](https://github.com/tscnlab/LightLogR/issues?q=label%3Abug).
- Also make sure to search the internet (including Stack Overflow) to see if users outside of the GitHub community have discussed the issue.
- Collect information about the bug:
- Stack trace (Traceback)
- OS, Platform and Version (Windows, Linux, macOS, x86, ARM)
- Version of the LightLogR
- Possibly your input and the output
- Can you reliably reproduce the issue? And can you also reproduce it with older versions?

<!-- omit in toc -->
#### How Do I Submit a Good Bug Report?

You must never report security related issues, vulnerabilities or bugs including sensitive information to the issue tracker, or elsewhere in public. Instead sensitive bugs must be sent by email to the [lead developer](mailto:johannes.zauner@tum.de).

We use GitHub issues to track bugs and errors. If you run into an issue with the project:

- Open an [Issue](https://github.com/tscnlab/LightLogR/issues/new). 
- We have an **issue template** for bugs that you can use when opening a new issue.
- Explain the behavior you would expect and the actual behavior.
- Please provide as much context as possible and describe the *reproduction steps* that someone else can follow to recreate the issue on their own. This usually includes your code. For good bug reports you should isolate the problem and create a reduced test case.
- Provide the information you collected in the previous section.

Once it's filed:

- The project team will label the issue accordingly.
- A team member will try to reproduce the issue with your provided steps. If there are no reproduction steps or no obvious way to reproduce the issue, the team will ask you for those steps and mark the issue as `needs-repro`. Bugs with the `needs-repro` tag will not be addressed until they are reproduced.
- If the team is able to reproduce the issue, it will be marked `needs-fix`, as well as possibly other tags (such as `critical`), and the issue will be left to be [implemented by someone](#your-first-code-contribution).

### Suggesting Enhancements

This section guides you through submitting an enhancement suggestion for LightLogR, **including completely new features and minor improvements to existing functionality**. Following these guidelines will help maintainers and the community to understand your suggestion and find related suggestions.

<!-- omit in toc -->
#### Before Submitting an Enhancement

- Make sure that you are using the latest version.
- Read the [documentation](https://tscnlab.github.io/LightLogR/) carefully and find out if the functionality is already covered, maybe by an individual configuration.
- Perform a [search](https://github.com/tscnlab/LightLogR/issues) to see if the enhancement has already been suggested. If it has, add a comment to the existing issue instead of opening a new one.
- Find out whether your idea fits with the scope and aims of the project. It's up to you to make a strong case to convince the project's developers of the merits of this feature. Keep in mind that we want features that will be useful to the majority of our users and not just a small subset. If you're just targeting a minority of users, consider writing an add-on/plugin library.

<!-- omit in toc -->
#### How Do I Submit a Good Enhancement Suggestion?

Enhancement suggestions are tracked as [GitHub issues](https://github.com/tscnlab/LightLogR/issues).

- We have an **issue template** for enhancement suggestions that you can use when opening a new issue.
- Use a **clear and descriptive title** for the issue to identify the suggestion.
- Provide a **step-by-step description of the suggested enhancement** in as many details as possible.
- **Describe the current behavior** and **explain which behavior you expected to see instead** and why. At this point you can also tell which alternatives do not work for you.
- You may want to **include screenshots or screen recordings** which help you demonstrate the steps or point out the part which the suggestion is related to. You can use [LICEcap](https://www.cockos.com/licecap/) to record GIFs on macOS and Windows, and the built-in [screen recorder in GNOME](https://help.gnome.org/users/gnome-help/stable/screen-shot-record.html.en) or [SimpleScreenRecorder](https://github.com/MaartenBaert/ssr) on Linux. <!-- this should only be included if the project has a GUI -->
- **Explain why this enhancement would be useful** to most LightLogR users. You may also want to point out the other projects that solved it better and which could serve as inspiration.

### Your First Code Contribution

LightLogR is written in R, using the [devtools](https://devtools.r-lib.org) package. We are using the [RStudio](https://posit.co/download/rstudio-desktop/) IDE from Posit, but feel free to use a different IDE.

You can suggest changes or additions to the code via pull requests. Pull requests have to be reviewed and approved by at least one reviewer, and can then be merged into the main project branch. Please use the `main` branch as a basis for pull requests.

<!-- omit in toc -->
#### Pull request process

- We recommend that you create a Git branch for each pull request (PR).

- New code should follow the [tidyverse style guide](http://style.tidyverse.org/). 

- You can use the [styler](https://cran.r-project.org/package=styler) package to apply these styles, but please don’t restyle code that has nothing to do with your PR.

- We use [roxygen2](https://roxygen2.r-lib.org), with [Markdown](https://roxygen2.r-lib.org/articles/rd-formatting.html) syntax, for documentation.

- We use [testthat](https://cran.r-project.org/package=testthat). Contributions with test cases included are easier to accept.

- For user-facing changes, add a bullet to the top of NEWS.md below the current development version header describing the changes made followed by your GitHub username, and links to relevant issue(s)/PR(s).

<!-- omit in toc -->
#### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using the GitHub web interface, so long as the changes are made in the source file.

YES: you edit a roxygen comment in a .R file below R/.

NO: you edit an .Rd file below man/.

<!-- omit in toc -->
#### Prerequisites

Before you make a substantial pull request, you should always file an issue and make sure someone from the team agrees that it’s a problem. If you’ve found a bug, create an associated issue and illustrate the bug with a minimal reprex.

## Styleguides
### Commit Messages
Commits should contain small changes that can be judged on their own. I.e., approval of a commit should not depend on some yet to be written code and functionality. Commit messages should state why these changes were necessary/helpful, and, if changes are complex, what they accomplish.

## Join The Project Team

You can join the project team simply by contributing to the package. This can take the form of new or improved functions, documentation, or unit tests. If you are eager to join but unsure what to do, contact the [lead developer](mailto:johannes.zauner@tum.de).

<!-- omit in toc -->
## Attribution
This guide is based on the [contributing.md](https://contributing.md/generator)!
Parts of the guide were taken from the [pkgdown contributing.md](https://pkgdown.r-lib.org/CONTRIBUTING.html).