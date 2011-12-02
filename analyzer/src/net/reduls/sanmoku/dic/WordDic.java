package net.reduls.sanmoku.dic;

import java.util.List;

public final class WordDic {
    public static interface Callback {
        public void call(ViterbiNode vn, boolean isSpace);
        public boolean isEmpty();
    }

    public static void search(String text, int start, Callback fn) {
        SurfaceId.eachCommonPrefix(text, start, fn);
    }

    public static void eachViterbiNode(Callback fn, int surfaceId, 
                                       int start, int length, boolean isSpace) {
        for(Morpheme.Entry e : Morpheme.getMorphemes(surfaceId))
            fn.call(new ViterbiNode(start, (short)length,
                                    e.cost, e.posId,
                                    e.morphemeId),
                    isSpace);
    }
}