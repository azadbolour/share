#!/bin/sh

package=ModuloNat.ipkg

idris --clean $package
idris --build $package
