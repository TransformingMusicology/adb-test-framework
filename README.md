## AudioDB Test Framework

A tool for automating tests of the libaudioDB feature searching
database system.

### Installation

1. Install [`libaudioDB`](https://github.com/TransformingMusicology/libaudioDB) and [`libaudioDB-haskell`](https://github.com/TransformingMusicology/libaudioDB-haskell).

2. Clone the repository:

        $ git clone https://github.com/TransformingMusicology/adb-test-framework.git

3. Create a Cabal sandbox:

        $ cd adb-test-framework
        $ cabal sandbox init

4. If you haven't installed `libaudioDB-haskell` somewhere where `GHC`
   will look for it, you can point `cabal` to your local copy:

        $ cabal sandbox add-source /path/to/libaudioDB-haskell

5. Install the dependencies:

        $ cabal install --only-dependencies

   (If you did step 4, this should also build and install
   `libaudioDB-haskell` in your sandbox.)

6. Build:

        $ cabal build

### Usage

You define a test in a YAML-formatted configuration file. In
`examples/example-config.yaml` you will find an example of a test
configuration file.

Each file contains exactly one "test" which has an `identifier` value
and one or more `queries`.

#### Queries

Each query has the properties:

<dl>
  <dt>idenitifier</dt>
  <dd>A unique string identifying the query.</dd>
  <dt>db</dt>
  <dd>The database against which the query will be made.</dd>
  <dt>query</dt>
  <dd>The query that will be executed.</dd>
  <dt>specifiedBy</dt>
  <dd>The name of the entity who specified the query (e.g. a person).</dd>
  <dt>evlaution</dt>
  <dd>The method that will be used to evaluate the results against the required results: <strong><code>distance</code></strong>: requires that the distances of each result are within some given threshold of the required results; <strong><code>order</code></strong>: requires that the results are in the same order as the required results (but disregards the actual distances).</dd>
  <dt>requiredResults</dt>
  <dd>A list of results that are expected for the query.</dd>
</dl>

#### Databases

The database is specified with the following properties:

<dl>
  <dt>filename</dt>
  <dd>The filename of the audioDB database file.</dd>
  <dt>feature</dt>
  <dd>A description of the audio feature this database contains.</dd>
  <dt>sampleRate</dt>
  <dd>The sample rate at which the audio files contained in the database were encoded.</dd>
  <dt>stepSize</dt>
  <dd>The step size used by the feature extractor, i.e. how many samples are in each feature.</dd>
  <dt>power</dt>
  <dd>Whether the database includes power features (true|false)</dd>
</dl>

#### Query configuration

The query is specified with the following properties:

<dl>
  <dt>type</dt>
  <dd>One of: <strong><code>point</code></strong>; <strong><code>track</code></strong>; <strong><code>sequence</code></strong>; or <strong><code>nsequence</code></strong></dd>
  <dt>key</dt>
  <dd>The key of the track from the database from which the query sequence will be drawn.</dd>
  <dt>start</dt>
  <dd>The start point in the query track of the query sequence; in seconds, encoded as a double.</dd>
  <dt>length</dt>
  <dd>The length of the query sequence; in seconds, encoded as a double.</dd>
  <dt>pointNN</dt>
  <dd>(optional) When executing an <strong><code>nsequence</code></strong>-type query, how many result sequences should be included from each track?</dd>
  <dt>radius</dt>
  <dd>(optional) For radius searches; purpose unknown to author at time of writing <code>;-)</code></dd>
  <dt>resultLen</dt>
  <dd>How many tracks should be included in the results?</dd>
  <dt>sequenceHop</dt>
  <dd>(optional) When executing an <strong><code>nsequence</code></strong>-type query, skip this number of sequences between each sequence searched for.</dd>
  <dt>absoluteThreshold</dt>
  <dd>(optional) When power features are included, disregard any result sequences whose power values (i.e. loudness) are below this threshold; in dB, encoded as a double.</dd>
  <dt>relativeThreshold</dt>
  <dd>(optional) When power features are included, only consider sequences as matches if their power values (i.e. loudness) is equal to this value; in dB, encoded as a double.</dd>
  <dt>unit-norming</dt>
  <dd>(optional) Use unit norming (true|false)</dd>
  <dt>distance</dt>
  <dd>The distance measure to be used. One of: <strong><code>dot-product</code></strong>; <strong><code>euclidean-normed</code></strong>; <strong><code>euclidean</code></strong>; <strong><code>kullback</code></strong>.</dd>
  <dt>rotations</dt>
  <dd>(optional) A list of rotations which the query should undergo. The query will be repeated as many times as there are numbers in this list, with each repetition using the query features rotated by the number of bins indicated by the number in the list.</dd>
</dl>

#### Results specification

The required results are specified as a list of objects with each having the following properties:

<dl>
  <dt>key</dt>
  <dd>The key of the track from which the result comes.</dd>
  <dt>distance</dt>
  <dd>The distance value of the result; encoded as a double.</dd>
  <dt>distThresh</dt>
  <dd>(optional) A threshold within which the distance value must lie; i.e. the distance of a correct match sequence must be <code>distance</code> ± <code>distThresh</code>; encoded as a double.</dd>
  <dt>start</dt>
  <dd>The starting position of the result sequence in the track; in seconds, encoded as a double.</dd>
  <dt>startThresh</dt>
  <dd>(optional) A theshold within which the start position must lie; i.e. the start of a correct match sequence must be at <code>start</code> ± <code>startThresh</code>; in seconds, encoded as a double.</dd>
  <dt>length</dt>
  <dd>(optional) The length of the result sequence; in seconds, encoded as a double.</dd>
  <dt>lengthThresh</dt>
  <dd>(optional) A threshold within which the length of the result must lie; in seconds, encoded as a double.</dd>
</dl>

### Executing the test

The build process creates an executable called `adbtest`. Run this
with the `-c-` (or `--config`) option followed by the filename of your
test configuration. You can optionally use the `-n` (or `--dry-run`)
option to prevent actually executing the test and just check that the
configuration file is correct.

### License

Copyright (C) 2015, 2016 Richard Lewis, Goldsmiths' College

Author: Richard Lewis <richard.lewis@gold.ac.uk>

This file is part of AudioDBTest.

AudioDBTest is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

AudioDBTest is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with AudioDBTest. If not, see <http://www.gnu.org/licenses/>.
