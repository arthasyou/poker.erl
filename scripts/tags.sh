#!/usr/bin/env bash

cd ..
# /Users/youshuxiang/.vim/bundle/vim-erlang-tags/bin/vim_erlang_tags.erl

ctags --languages=erlang --erlang-kinds=-dr -R -f tags /usr/local/Cellar/erlang/22.3.1/lib/erlang/lib

