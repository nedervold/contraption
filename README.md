# contraption

A tool for creating and testing parsers and prettyprinters for
user-defined languages.  The name "contraption" comes from the fact
that it uses a motley collection of formats to store the configuration.

The new idea here is that a spreadsheet—or equivalently,
comma-separated values—is a useful format for storing very sparse
data.  When displayed in a spreadsheet, it's easy for a user to
visually scan to find information, while it would be tremendously
confusing to read most formats, like JSON or YAML where the
boilerplate of the format would swamp the few significant values.
