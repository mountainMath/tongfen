# Agentic Coder Instructions for tongfen Package

## Package Overview

**tongfen** is an R package for harmonizing geographic data across different boundaries, primarily used for making Canadian and US census data from different years comparable. The package uses a "least common denominator" approach to create a common geography through spatial operations.

## Core Architecture

- **Main functions**: `get_tongfen_correspondence()`, `tongfen_aggregate()`, `tongfen_estimate()`
- **Key dependencies**: dplyr (≥1.0), tidyr, sf (spatial features)
- **Data structures**: Tibbles with spatial geometries (sf objects)
- **Computational focus**: Spatial operations, data aggregation, correspondence generation

## Performance Optimization Guidelines

### Critical Performance Patterns

1. **Avoid nested loops with grouped operations**
   - The package previously had nested while/for loops with `group_by()` - extremely inefficient
   - Always vectorize when possible
   - Pre-compute grouping keys when iterations are needed

2. **Spatial operations are expensive**
   - Minimize redundant spatial intersections
   - Use spatial indexing when available
   - Avoid repeated geometry transformations
   - Cache spatial results when safe to do so

3. **String operations in hot paths**
   - Avoid `paste0()` in mutate inside grouped data
   - Pre-compute unique values before string operations
   - Use vectorized operations, not loops

4. **Modern dplyr patterns**
   - Use `mutate(across(...))` instead of loops with mutate
   - Use `summarize(across(...))` instead of deprecated `summarize_at`
   - Combine operations in single mutate when possible

### Benchmarking Requirements

**All performance changes MUST be validated with microbenchmark:**

```r
library(microbenchmark)

# Benchmark template
mb_result <- microbenchmark(
  old_approach = { ... },
  new_approach = { ... },
  times = 100,
  unit = "ms"
)

print(mb_result)
summary(mb_result)
```

**Benchmark artifacts:**
- Store in `benchmarks/` directory (NOT tracked in git, in .Rbuildignore)
- Document setup, results, and conclusions
- Include representative data sizes (city-level, ~1000-10000 polygons)
- Test with multiple census years when relevant

### Testing Requirements

1. **Correctness first**: Performance improvements must not change results
2. **Exact equality**: Use `all.equal()` or `identical()` to verify outputs
3. **Edge cases**: Test with empty geometries, single regions, large datasets
4. **Backwards compatibility**: No breaking changes to public APIs

### Code Style Conventions

1. **tidyverse style**: Follow tidyverse style guide
2. **Piping**: Use `%>%` for clarity, but avoid excessive chaining (>10 operations)
3. **Function length**: Keep functions focused; extract helper functions when >50 lines
4. **Documentation**: Update roxygen2 docs for any signature changes
5. **Naming**: Use snake_case, descriptive names

### Common Pitfalls to Avoid

1. **Don't optimize prematurely**: Profile first, optimize bottlenecks
2. **Avoid breaking sf objects**: Spatial tibbles need special handling
3. **Watch for NSE**: Use `!!`, `.data$`, `all_of()` properly with dplyr
4. **Memory vs speed**: This package handles large spatial data - balance both
5. **R build system**: Ensure benchmarks excluded via .Rbuildignore
6. **Dependencies**: Don't add heavy dependencies without discussion

### Spatial Data Considerations

1. **Geometry validity**: Always check `st_is_valid()` after operations
2. **CRS consistency**: Ensure coordinate reference systems match
3. **Empty geometries**: Handle `st_is_empty()` cases gracefully
4. **Buffering**: Use appropriate tolerances for topology fixes
5. **Precision**: Spatial operations accumulate floating-point errors

### Performance Tier System

**Tier 1 (Critical)**: 5-20x speedup potential
- Nested loop optimization
- Redundant computation elimination
- Spatial operation efficiency

**Tier 2 (High)**: 2-5x speedup potential
- Vectorization of loops
- Modern dplyr patterns
- Eliminating deprecated functions

**Tier 3 (Medium)**: 1.5-2x speedup potential
- Parallelization (use with caution - spatial ops already use C++)
- Caching strategies
- Data structure optimization

### Workflow Reference

See example usage: https://github.com/mountainMath/doodles/blob/master/content/posts/2019-06-04-multi-census-tongfen.Rmarkdown

**Typical use case:**
- City-level analysis (Toronto, Vancouver, etc.)
- 4+ census years (2001, 2006, 2011, 2016, 2021)
- DA-level resolution (thousands of small polygons)
- Multiple census variables per year

### Git Workflow

1. **Branches**: Use `perf/` prefix for performance work
2. **Commits**: Clear, descriptive messages; one logical change per commit
3. **Testing**: Validate before committing
4. **Documentation**: Update this file when adding new patterns

### Package Build

**Always verify package still builds:**
```bash
R CMD build .
R CMD check --as-cran tongfen_*.tar.gz
```

**Key build files:**
- `.Rbuildignore`: Excludes benchmarks/, claude/ from package
- `.gitignore`: Excludes benchmarks/ from version control
- `DESCRIPTION`: Includes microbenchmark in Suggests

---

## Improvement History

### Tier 1 Performance Improvements (2025-11)

1. **Optimized `get_tongfen_correspondence` nested loops** (R/helpers.R:40-50)
   - Before: Nested while/for loops with repeated group_by operations
   - After: Vectorized graph-based approach with iteration limit
   - Impact: 5-20x speedup for correspondence generation

2. **Vectorized TongfenUID string generation** (R/helpers.R:57-63)
   - Before: For loop with repeated paste0/sort in grouped mutate
   - After: Single vectorized mutate with pre-computed values
   - Impact: 3-10x speedup for UID generation

3. **Optimized spatial intersections** (R/tongfen.R:459-474)
   - Before: Two separate intersection operations, multiple conversions
   - After: Single intersection with efficient format handling
   - Impact: 2x speedup for spatial operations

4. **Vectorized variable scaling loops** (R/tongfen.R - multiple locations)
   - Before: For loops with repeated mutate calls
   - After: `mutate(across(all_of(...), ...))`  pattern
   - Impact: 2-3x speedup for scaling operations

---

**Last updated**: 2025-11-16
**Maintainer contact**: Jens von Bergmann (jens@mountainmath.ca)
