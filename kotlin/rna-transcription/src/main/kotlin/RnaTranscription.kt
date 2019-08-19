fun transcribeToRna(dna: String): String =
        dna.fold(String()) { acc, nucleotide ->
            acc + when (nucleotide) {
                'A' -> 'U'
                'C' -> 'G'
                'G' -> 'C'
                'T' -> 'A'
                else -> throw IllegalArgumentException("DNA may only contain nucleotides in A, C, G and T")
            }
        }
