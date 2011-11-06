package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Morpheme {
    public static final int[] idMorphmesMap;
    public static final byte[] morphemeInfos; 

    static {
        long beg_t = java.lang.System.currentTimeMillis();
        System.out.println("#START-3");

        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("id-morphemes-map.bin");
            final int idMorphmesMapSize = Misc.readInt(in);
            idMorphmesMap = new int[idMorphmesMapSize];
            
            final byte[] buf = new byte[idMorphmesMapSize];
            try {
                in.readFully(buf, 0, buf.length);
                idMorphmesMap[0] = 0;
                for(int i=1; i < buf.length; i++) 
                    idMorphmesMap[i] = buf[i-1] + idMorphmesMap[i-1];
            } catch(Exception e) {}
            Misc.close(in);
        }

        {
            DataInputStream in = Misc.openDictionaryDataAsDIS("morpheme.bin");
            final int morphemeCount = Misc.readInt(in);
            
            morphemeInfos = new byte[morphemeCount*4];
            try {
                in.readFully(morphemeInfos, 0, morphemeInfos.length);
            } catch(Exception e) {}
            
            Misc.close(in);
        }
        System.out.println("#END-3: " + (java.lang.System.currentTimeMillis()-beg_t));
    }

    public static int morphemesBegin(int surfaceId) {
        return idMorphmesMap[surfaceId];
    }

    public static int morphemesEnd(int surfaceId) {
        return idMorphmesMap[surfaceId+1];
    }

    public static short posId(int morphemeId) {
        return (short)((morphemeInfos[morphemeId*4+0]<<8) | (morphemeInfos[morphemeId*4+1]&0xff));
    }

    public static short cost(int morphemeId) {
        return (short)((morphemeInfos[morphemeId*4+2]<<8) | (morphemeInfos[morphemeId*4+3]&0xff));
    }
}