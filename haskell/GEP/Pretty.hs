module GEP.Pretty (
configDoc
) where


import qualified Data.Map as M
import Text.PrettyPrint.ANSI.Leijen

import GEP.Types
import GEP.Config

dcDomainDoc :: Config -> Doc
dcDomainDoc c = if rncs > 0
    then 
      nest 2 (
        text "Dc Domain:" <+> green (text "active" <+> 
          int rncs <+> text "RNCs") <$$>
        text "Alphabet:" <+> alphabet <$$>
        text "RNC Range:" <+> double l <+> text "->" <+> double h
        )
    else
      text "Dc Domain:" <+> red (text "inactive")
  where 
    rncs = M.size .unDCMap $ dcAlphabet c
    alphabet = text . M.keys . unDCMap $ dcAlphabet c
    (l,h) = unPair $ rangeRNC c


geneDoc :: Config -> Doc
geneDoc c = nest 2 (
    bold (text "Gene Layout:" <+> lengths) <$$>
    dcDomainDoc c <$$>
    text "Operators:" <+> list (map char ops) <$$>
    text "Terminals:" <+> list (map term terms)
    )
  where
    gl = geneLength c
    hl = headLength c
    tl = tailLength c
    dcl = dcLength c
    ops = M.keys $ operators c
    lengths = int gl <+> text "(" <> int hl <> text "-" <>
      int tl <> text "-" <> int dcl <> text ") -> Double"
    term (ch, Value v) = char ch <> char '=' <> double v
    term (_, RNC) =  text "?=RNC"
    terms = M.toList . unTermMap $ terminals c

modDoc :: Config -> Doc
modDoc c = nest 2 (
    bold (text "Modification Rates:") <$$>
    text "Mutation:" <+> double (mutationRate c)
    )


configDoc :: Config -> Doc
configDoc c = nest 2 (
    by (text "GEP CONFIGURATION:" <+> n) <$$>
    bold (text "No. Chromosomes:" <+> int noc) <$$>
    bold (text "Genes per Chromosome:" <+> int gpc) <$$>
    geneDoc c <$$>
    modDoc c
    ) <> line <> line
  where
    n = text $ name c
    noc = noChromosomes c
    gpc = genesPerChromosome c
    by = bold . yellow

main :: IO ()
main = putStr "[GEP.Pretty] Done!"
  
