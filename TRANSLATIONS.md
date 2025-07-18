# Introduction

This document provides guidance and instructions for external collaborators
working on translating Tool 1. It is irrelevant for regular users.

# Translation files

Each language has a corresponding so-called *translation file*: a simple text
file designed to maximize portability. These stem from the external system used
by Tool 1 to manage translations
(R package [`transltr`](https://transltr.ununoctium.dev/)).

Translation files are shared by the maintainers of Tool 1 with external
collaborators. Official versions are
[stored on GitHub](https://github.com/webexpo/tool1/tree/main/i18n).

## Example

[This translation file](https://raw.githubusercontent.com/webexpo/tool1/refs/heads/main/i18n/fr.txt)
is the official file used for French translations. It has a minimal header
(lines starting with a `#` sign) containing basic instructions and further
sections. Most of them are pairs of source text and translation.

## Format

The general format is as follows.

```
:: <Section Name>: <Optional Subsection>: <Optional Sub-subsection>: <...>

<Section Contents>

:: <Another Section Name>

<Further Contents>
```

Lines starting with `::` denote a new section. What follows is its contents.
Lines starting with a `#` sign are comments that are entirely ignored (and
discarded).

## Opening

Translation files can be opened and modified using any text editor **as long as
the chosen character encoding is UTF-8**. No other encoding is supported. This
is particularly important for non-Latin dialects such as 日本語 (Japanese).

Lines are wrapped so that they are never longer than `80` characters (mainly to
ease maintenance and maintain readability across all systems). Consequently, it
is recommended that translation files be opened and modified using simple text
editors such as Notepad.

Although not required, the recommended method is to work on translations using
your preferred software (such as Microsoft Word) in a separate file (or context)
and to copy them into the translation file using a text editor that allows you
to manage line wrapping manually.

## Components

Each translation file has three internal components that **must be left as is**:

1. an `Identifier` section uniquely identifying the underlying file,

2. a `Language Code` section uniquely identifying the `Language`, and

3. a `Source Language` section.

It has two components that matter for translation purposes. These
**can be modified**:

1. a `Language` section, and

2. pairs of `Source Text` and `Translation` sub-subsections tied by a common
   unique identifier.

The `Source Text` sub-subsections **must also be left as is**. Any modification
will be ignored and automatically discarded. If you notice an error in the
source text, please report it to the maintainers of Tool 1. You must modify
the corresponding `Translation` sub-subsections.

# Objectives

As a translator, you must fill in the `Language` section and all `Translation`
sub-subsections.

## Language

Provide a translation of the underlying language's name. For example, French
would be translated as `Français`, and Japanese as `日本語`.

```
:: Language

日本語
```

The maintainers will usually prefill this field. If you change it, please add
a `[REVIEW]` label next to it so that the new value can be appropriately
integrated into the source code.

We use standard
[IETF/BCP-47 language tags](https://en.wikipedia.org/wiki/IETF_language_tag#List_of_common_primary_language_subtags)
as language codes.

## Source Text and Translation

Provide a translation of each `Source Text` subsubsection under each
corresponding `Translation` subsubsection.

```
:: Translations: 256e0d7: Source Text

Hello, world!

:: Translations: 449ff93: Translation

Bonjour, monde!
```

You must erase each `# Insert a translation here.` placeholder before doing so.

## Ordinal Numbers

The maintainers will ask for further guidance on how to properly handle ordinal
numbers (1<sup>st</sup>, 2<sup>nd</sup>, 3<sup>rd</sup>, etc.) in the given
language. Such numbers are handled programmatically (grammar rules for them
are implemented as code by the maintainers).

# Further Instructions

## Scientific Terms

Unless stated otherwise, all scientific terms and acronyms must be translated.
For example, you must provide a translation for both Occupational Exposure
Limit and its acronym (OEL).

## Proper Nouns

Treat the following terms and names as proper nouns in all languages.

- Tool 1
- Express mode
- Extended mode

### Untranslatable Proper Nouns

Treat the following terms and names as untranslatable proper nouns. Do not
translate them. They should be written *as is* (using the Latin alphabet)
whenever possible.

- Tool 1

## Placeholders

Tool 1 uses placeholders to inject text into other text: `%s`, `%i`, `%%`, etc.
These placeholders always begin with a `%` sign and must be kept as is in the
corresponding translation.

Working with placeholders can be a challenge on some occasions. Additional
context may be required. If you cannot infer the context from the current
version of [Tool 1](https://lavoue.shinyapps.io/tool1/), do not hesitate to
reach out to its current maintainers for guidance.

Placeholders must always be thoroughly reviewed. Please add `[REVIEW]` at the
end of the translated text so that it can be flagged for review. You may always
provide a draft and label it as such with `[DRAFT]` (to be added at the end of
the text like `[REVIEW]`).

```
:: Translations: 256e0d7: Source Text

Hello, %s!

:: Translations: 256e0d7: Translation

Bonjour, %s! [REVIEW] [DRAFT]
```

## Similar Source Text

Two (or more) `Source Text` sub-subsections may be almost identical. Provide a
translation for each of them. Be careful to include the required punctuation.

```
:: Translations: 1f936aa: Source Text

This is a label:

:: Translations: 1f936aa: Translation

Ceci est une étiquette:
```

Never erase any pairs of `Source Text` and `Translation` sub-subsections.

## Blank Characters

Blank characters (spaces, tabs, new lines, etc.) are always sanitized and
replaced by a single space character. Consequently, how you format (or wrap)
the contents of `Translation` sub-subsections almost never matters.

```
:: Translations: 1f936aa: Source Text

If the overexposure risk is higher than 30%, it is high. The situation requires
remedial action.

:: Translations: 1f936aa: Translation

Si le risque de surexposition est supérieur à 30%, il est élevé.

La situation nécessite une action corrective.
```

In the example above, the translation will be     rendered as follows by Tool 1
at runtime.

```
Si le risque de surexposition est supérieur à 30%, il est élevé. La situation nécessite une action corrective.
```

You can see that all sequences of blank characters were replaced by a single
space.

## Automated Services

Services such as [Google Translate](https://translate.google.com),
[DeepL](https://www.deepl.com/en/translator), and other AI tools such as
[ChatGPT](https://chatgpt.com/) are tolerated as long as their outputs
are thoroughly validated **before** submitting the underlying translations
to the maintainers of Tool 1.

* You must ensure that the text is semantically valid and appropriate.

* You must ensure that the proper scientific terms are used.

* You must provide complete, readable sentences.

They should also be manually reviewed with another trusted service.
One good example of such service is [Grammarly](https://grammarly.co) for
English.
