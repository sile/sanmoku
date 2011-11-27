package net.reduls.sanmoku.bin;

import java.io.IOException;
import net.reduls.sanmoku.Tagger;
import net.reduls.sanmoku.Morpheme;
import net.reduls.sanmoku.FeatureEx;
import net.reduls.sanmoku.util.ReadLine;

public final class SanmokuFeatureEx {
    public static void main(String[] args) throws IOException {
        if(args.length!=0) {
	    System.err.println("Usage: java net.reduls.igo.bin.SanmokuFeatureEx");
	    System.exit(1);
	}

	final ReadLine rl = new ReadLine(System.in);
        for(String s=rl.read(); s != null; s=rl.read()) {
            for(Morpheme m : Tagger.parse(s)) {
                FeatureEx fe = new FeatureEx(m);
                String baseform = fe.baseform.length()==0 ? "*" : fe.baseform;
                String reading = fe.reading.length()==0 ? "*" : fe.reading;
                String pronunciation = fe.pronunciation.length()==0 ? "*" : fe.pronunciation;
                System.out.println(m.surface+"\t"+m.feature+","+baseform+","+reading+","+pronunciation);
            }
            System.out.println("EOS");
        }
    }   
}