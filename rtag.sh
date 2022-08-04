#!/bin/bash -ex
VER=$(git log -n 1 --date=format:%Y%m%d --pretty=format:3.%cd.%h)
7z x sireum-v3-intellij.zip sireum-v3-intellij/lib/sireum-v3-intellij.jar
7z x sireum-v3-intellij/lib/sireum-v3-intellij.jar META-INF/plugin.xml
sed -i.bak "s/3.X.Y/${VER}/g" META-INF/plugin.xml
7z a sireum-v3-intellij/lib/sireum-v3-intellij.jar META-INF/plugin.xml
7z a sireum-v3-intellij.zip sireum-v3-intellij/lib/sireum-v3-intellij.jar
rm -fR META-INF sireum-v3-intellij
echo "Tagging ${VER}"
git tag ${VER}
git push origin ${VER}