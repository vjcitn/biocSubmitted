#!/usr/bin/env python3
"""
Semantic Term Matcher for Bioinformatics

Finds the N most semantically similar terms from a target list
for each term in a source list.

Two modes available:
  1. LIGHTWEIGHT: TF-IDF + character n-grams (no external models needed)
  2. DEEP LEARNING: BioBERT/PubMedBERT embeddings (best quality, requires model download)
"""

import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from typing import List, Dict, Optional
import json
import re


# ============================================================
# METHOD 1: LIGHTWEIGHT (TF-IDF + N-GRAMS) - No model download
# ============================================================

def preprocess_term(term: str) -> str:
    """Normalize bioinformatics terms for better matching."""
    term = term.lower()
    # Expand common abbreviations
    expansions = {
        'dna': 'deoxyribonucleic acid',
        'rna': 'ribonucleic acid',
        'mrna': 'messenger ribonucleic acid',
        'trna': 'transfer ribonucleic acid',
        'snp': 'single nucleotide polymorphism',
        'gwas': 'genome wide association study',
        'pcr': 'polymerase chain reaction',
        'ngs': 'next generation sequencing',
    }
    for abbr, full in expansions.items():
        term = re.sub(rf'\b{abbr}\b', f'{abbr} {full}', term)
    return term


def find_closest_terms_tfidf(
    source_terms: List[str],
    target_terms: List[str],
    top_n: int = 3
) -> Dict:
    """
    Find semantically similar terms using TF-IDF with character n-grams.
    Works offline without any model downloads.
    
    Args:
        source_terms: List of terms to find matches for
        target_terms: List of candidate terms to match against
        top_n: Number of closest matches to return
        
    Returns:
        Dictionary mapping each source term to its top_n matches with scores
    """
    # Preprocess all terms
    source_processed = [preprocess_term(t) for t in source_terms]
    target_processed = [preprocess_term(t) for t in target_terms]
    
    # Combine for fitting the vectorizer
    all_terms = source_processed + target_processed
    
    # Use character n-grams (3-5) + word n-grams for morphological similarity
    vectorizer = TfidfVectorizer(
        analyzer='char_wb',
        ngram_range=(3, 5),
        min_df=1,
        max_features=10000
    )
    
    # Fit and transform
    vectorizer.fit(all_terms)
    source_vectors = vectorizer.transform(source_processed)
    target_vectors = vectorizer.transform(target_processed)
    
    # Calculate cosine similarities
    similarities = cosine_similarity(source_vectors, target_vectors)
    
    results = {}
    for i, source_term in enumerate(source_terms):
        top_indices = np.argsort(similarities[i])[::-1][:top_n]
        
        matches = [
            {
                "term": target_terms[idx],
                "similarity_score": round(float(similarities[i][idx]), 4)
            }
            for idx in top_indices
        ]
        results[source_term] = matches
    
    return results


# ============================================================
# METHOD 2: DEEP LEARNING (BioBERT) - Best quality
# ============================================================

def find_closest_terms_embeddings(
    source_terms: List[str],
    target_terms: List[str],
    top_n: int = 3,
    model_name: str = "pritamdeka/S-PubMedBert-MS-MARCO"
) -> Dict:
    """
    Find semantically similar terms using biomedical transformer embeddings.
    Requires sentence-transformers and internet access for model download.
    
    Recommended models for bioinformatics:
      - "pritamdeka/S-PubMedBert-MS-MARCO" (best for biomedical)
      - "microsoft/BiomedNLP-PubMedBERT-base-uncased-abstract"
      - "dmis-lab/biobert-base-cased-v1.2"
      - "all-MiniLM-L6-v2" (fast, general purpose)
    
    Args:
        source_terms: List of terms to find matches for
        target_terms: List of candidate terms to match against
        top_n: Number of closest matches to return
        model_name: HuggingFace model identifier
        
    Returns:
        Dictionary mapping each source term to its top_n matches with scores
    """
    from sentence_transformers import SentenceTransformer
    
    print(f"Loading model: {model_name}...")
    model = SentenceTransformer(model_name)
    
    print("Computing embeddings for source terms...")
    source_embeddings = model.encode(source_terms, show_progress_bar=True)
    
    print("Computing embeddings for target terms...")
    target_embeddings = model.encode(target_terms, show_progress_bar=True)
    
    print("Calculating similarities...")
    similarities = cosine_similarity(source_embeddings, target_embeddings)
    
    results = {}
    for i, source_term in enumerate(source_terms):
        top_indices = np.argsort(similarities[i])[::-1][:top_n]
        
        matches = [
            {
                "term": target_terms[idx],
                "similarity_score": round(float(similarities[i][idx]), 4)
            }
            for idx in top_indices
        ]
        results[source_term] = matches
    
    return results


# ============================================================
# UNIFIED INTERFACE
# ============================================================

def find_closest_terms(
    source_terms: List[str],
    target_terms: List[str],
    top_n: int = 3,
    method: str = "auto",
    model_name: Optional[str] = None
) -> Dict:
    """
    Main interface: find the top_n most semantically similar terms.
    
    Args:
        source_terms: List of terms to find matches for
        target_terms: List of candidate terms to match against  
        top_n: Number of closest matches to return (default: 3)
        method: "tfidf", "embeddings", or "auto" (tries embeddings, falls back to tfidf)
        model_name: For embeddings method, specify HuggingFace model
        
    Returns:
        Dictionary mapping each source term to its top_n matches with scores
    """
    if method == "tfidf":
        return find_closest_terms_tfidf(source_terms, target_terms, top_n)
    
    elif method == "embeddings":
        model = model_name or "pritamdeka/S-PubMedBert-MS-MARCO"
        return find_closest_terms_embeddings(source_terms, target_terms, top_n, model)
    
    elif method == "auto":
        try:
            model = model_name or "pritamdeka/S-PubMedBert-MS-MARCO"
            return find_closest_terms_embeddings(source_terms, target_terms, top_n, model)
        except Exception as e:
            print(f"Embeddings unavailable ({e}), falling back to TF-IDF...")
            return find_closest_terms_tfidf(source_terms, target_terms, top_n)
    
    else:
        raise ValueError(f"Unknown method: {method}. Use 'tfidf', 'embeddings', or 'auto'")


def print_results(results: Dict, title: str = "SEMANTIC MATCHING RESULTS") -> None:
    """Pretty print the matching results."""
    print("\n" + "=" * 70)
    print(title)
    print("=" * 70)

    for source_term, matches in results.items():
        print(f"\n📌 {source_term}")
        print("-" * 50)
        for rank, match in enumerate(matches, 1):
            score = match['similarity_score']
            bar = "█" * int(score * 20)
            print(f"   {rank}. {match['term']:<35} [{score:.3f}] {bar}")


# ============================================================
# EXAMPLE USAGE
# ============================================================
if __name__ == "__main__":
    # Example bioinformatics term lists
    list_1 = [
        "gene expression",
        "protein folding",
        "DNA methylation",
        "cell apoptosis",
        "RNA splicing"
    ]

    list_2 = [
        "transcription regulation",
        "mRNA processing",
        "histone modification",
        "programmed cell death",
        "protein structure prediction",
        "epigenetic modification",
        "gene transcription",
        "alternative splicing",
        "protein denaturation",
        "chromatin remodeling",
        "cell proliferation",
        "post-translational modification"
    ]

# Read file and strip whitespace from each line
with open('v2d.txt', 'r') as f:
    list_1 = [line.strip() for line in f]

with open('edamterms.txt', 'r') as f:
    list_2 = [line.strip() for line in f]

    print("=" * 70)
    print("BIOINFORMATICS SEMANTIC TERM MATCHER")
    print("=" * 70)
    print("\nSource terms (List 1):")
    for t in list_1:
        print(f"  • {t}")

    print("\nTarget terms (List 2):")
    for t in list_2:
        print(f"  • {t}")

    # Run with TF-IDF (works offline)
    print("\n🔍 Using TF-IDF method (lightweight, no model download)...")
    results = find_closest_terms(
        source_terms=list_1,
        target_terms=list_2,
        top_n=3,
        method="embeddings"
    )

    print_results(results)

    # Save results to JSON
    with open("matching_results.json", "w") as f:
        json.dump(results, f, indent=2)
    print("\n✓ Results saved to matching_results.json")
    
#    print("\n" + "-" * 70)
#    print("💡 For better accuracy with your own data, use method='embeddings'")
#    print("   which requires: pip install sentence-transformers")
#    print("   and internet access to download biomedical language models.")
#    print("-" * 70)
