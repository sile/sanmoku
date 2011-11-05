package net.reduls.gomoku.dic;

import java.io.DataInputStream;
import net.reduls.gomoku.util.Misc;

public final class Morpheme {

    public static final int[] idMorphmesMap;
    public static final short[] morphemes;

    static {
        DataInputStream in1 = Misc.openDictionaryDataAsDIS("id-morphemes-map.bin");
        DataInputStream in2 = Misc.openDictionaryDataAsDIS("morpheme.bin");

        idMorphmesMap = new int[Misc.readInt(in1)];
        morphemes = new short[Misc.readInt(in2)*2];
        
        for(int i=1; i < idMorphmesMap.length; i++)
            idMorphmesMap[i] = Misc.readByte(in1) + idMorphmesMap[i-1];
        
        for(int i=0; i < morphemes.length/2; i++) {
            morphemes[i*2] = Misc.readShort(in2);   // posId
            morphemes[i*2+1] = Misc.readShort(in2); // cost
        }
        
        Misc.close(in1);
        Misc.close(in2);
    }

    public static int morphemesBegin(int surfaceId) {
        return idMorphmesMap[surfaceId];
    }

    public static int morphemesEnd(int surfaceId) {
        return idMorphmesMap[surfaceId+1];
    }
}