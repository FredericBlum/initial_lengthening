# Word-initial consonant lengthening is a robust phenomenon across typologically diverse languages

## Reproduce results

Clone the necessary repositories:

```CLI
git clone https://github.com/cldf-datasets/doreco
git clone https://github.com/cldf-clts/clts
```

Download Doreco and convert to CLDF. While downloading, insert "Yes" when asked for ND-data. You do not need to download the audio files.

```
cd doreco/
pip install -e .
cldfbench download cldfbench_doreco.py
cldfbench makecldf cldfbench_doreco.py --glottolog-version v4.8
cldf createdb cldf/Generic-metadata.json doreco.sqlite
```

Run SQL:

```
sqlite3 -echo doreco.sqlite < etc/views.sql
cldfbench doreco.query --format tsv init_query.sql > scripts/data.tsv
```