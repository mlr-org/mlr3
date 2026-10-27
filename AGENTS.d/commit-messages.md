# Commit Messages

Follow the Conventional Commits specification.

## Format

```
<type>(<optional scope>): <description>

[optional body]

[optional footer(s)]
```

- The **description** must be a short summary immediately following the colon and space.
- The optional **body** provides additional contextual information, separated from the description by a blank line.
- The optional **footer** follows the git trailer format (e.g., `Fixes #123`), separated from the body by a blank line.

## Types

- **feat**: a new feature (correlates with MINOR in SemVer)
- **fix**: a bug fix (correlates with PATCH in SemVer)
- **docs**: changes to documentation only
- **style**: formatting, whitespace, etc.; no code change
- **refactor**: refactoring production code, e.g., renaming a variable
- **test**: adding or refactoring tests; no production code change
- **chore**: maintenance tasks; no production code change
- **perf**: performance improvements
- **ci**: changes to CI configuration
- **build**: changes to the build system or dependencies

## Breaking Changes

Add a `BREAKING CHANGE:` footer: `BREAKING CHANGE: parameter 'x' has been removed`.

## Rules

- Use the imperative mood in the description ("add feature", not "added feature").
- Do not capitalize the first word of the description.
- Do not end the description with a period.
- Keep the first line (type + scope + description) under 72 characters.
