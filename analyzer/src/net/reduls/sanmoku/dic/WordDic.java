package net.reduls.sanmoku.dic;

import java.util.List;

public final class WordDic {
    public static interface Callback {
        public void call(ViterbiNode vn);
        public boolean isEmpty();
    }

    public static void search(String text, int start, Callback fn) {
        SurfaceId.eachCommonPrefix(text, start, fn);
    }

    public static void eachViterbiNode(Callback fn, int surfaceId, 
                                       int start, int length, boolean isSpace) {
        final int i_start = Morpheme.morphemesBegin(surfaceId);
        final int i_end = Morpheme.morphemesEnd(surfaceId);
        for(int i=i_start; i < i_end; i++)
            fn.call(new ViterbiNode(start, (short)length,
                                    Morpheme.cost(i), Morpheme.posId(i),
                                    isSpace));
    }
}