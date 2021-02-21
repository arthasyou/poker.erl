#!/bin/bash

cd ../resource/proto
../../_build/default/lib/gpb/bin/protoc-erl ../proto/all_pb.proto

mv all_pb.erl ../../src/proto/
mv all_pb.hrl ../../include/