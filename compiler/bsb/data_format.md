

# format about lib/bs/.bsbuild

This file (in binary) contains all file info needed in build.

It is encoded in bsb_db_encode.ml and bsb_db_decode.ml, the format is optimized for performance in decoding where it is used most.


- The first 16 chars is digest of the following content

The rest is encoding for each group (source code and test), in most cases, you have one group or two groups (one for lib one for test).

Begining with a new line, the number of total groups (encoded in text format) are encoded.


For each group, it starts with a newline and the
number of modules (text format).

The following are list of modules in sorted order (Ext_string.compare) separated by newline.

The following are list of directories separated by tab character.

The next is a fixed length for module description, its encoding is hard coded in Bsb_db_encode.make_encoding