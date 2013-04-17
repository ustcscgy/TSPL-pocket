epub: build
	cd src && pandoc -S --epub-metadata=metadata.xml \
            --epub-cover-image=cover.png \
            --epub-stylesheet=tspl.css \
            -t epub3 -o ../build/TSPL.epub \
            tspl.md toc.md Preface.md Ch1.Introduction.md \
            Ch2.Getting.Started.md Ch3.Going.Further.md \
            Ch4.Procedures.and.Variable.Bindings.md \
            Ch5.Control.Operations.md Ch6.Objects.md \
            Ch7.Input.and.Output.md Ch8.Syntactic.Extension.md \
            Ch9.Records.md Ch10.Libraries.and.Top-Level.Programs.md \
            Ch11.Exceptions.and.Conditions.md \
            Ch12.Extended.Examples.md References.md \
            Answers.to.Selected.Exercises.md Formal.Syntax.md \
            Links.md
build:
	mkdir -p build
