-module(rna_transcription).

-export([to_rna/1]).

-spec to_rna(string()) -> string().
to_rna([]) -> [];
to_rna([$G | Rest]) -> [$C | to_rna(Rest)];
to_rna([$C | Rest]) -> [$G | to_rna(Rest)];
to_rna([$T | Rest]) -> [$A | to_rna(Rest)];
to_rna([$A | Rest]) -> [$U | to_rna(Rest)].
