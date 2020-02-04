
Anaconda
===
Tested on a recent Ubuntu installation.

Most of the book runs on python 3.4, with scikit-learn, Jupyter notebook, pandas, and their dependencies.
A simple setup using [Anaconda](https://www.continuum.io/why-anaconda) looks like this:

1. `conda create -n LDMbook python=3.4`
2. `source activate LDMbook`
3. `conda install scikit-learn`
4. `conda install jupyter`
5. `conda install pandas`
5. `conda install nltk`
6. `python -m nltk.downloader` (then choose "book" and click "Download")
7. `conda install matplotlib`
8. `conda install networkx`


Chapter 11
===

You'll need Theano, which you can get via:

`pip install --upgrade theano`

Do not use Anaconda to install, as the version is too old for Lasagne.

Then install Lasagne. First clone the repo with:

`git clone https://github.com/Lasagne/Lasagne.git`

Then, `cd Lasagne` to enter the directory and build with:

`python3 setup.py install`

Note that you should still have your source activated to do this!

Then, install nolearn from its git repo with:

`pip install git+https://github.com/dnouri/nolearn.git@master#egg=nolearn==0.7.git`
