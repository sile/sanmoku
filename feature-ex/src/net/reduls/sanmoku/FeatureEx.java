package net.reduls.sanmoku;

import net.reduls.sanmoku.util.Misc;

public final class FeatureEx {
    /**
     * static
     **/
    private static final byte[] info;
    private static final byte[] data;

    static {
        info = Misc.readBytesFromFile("feature.idx.bin", 6);
        data = Misc.readBytesFromFile("feature.text.bin", 2);
    }

    private static long info(int i) {
        return (((long)(info[i*6+0] & 0xff) << 40) | 
                ((long)(info[i*6+1] & 0xff) << 32) | 
                ((long)(info[i*6+2] & 0xff) << 24) |
                ((long)(info[i*6+3] & 0xff) << 16) |
                ((long)(info[i*6+4] & 0xff) <<  8) | 
                ((long)(info[i*6+5] & 0xff)));
    }

    private static String baseform(long info, Morpheme m) {
        final int baseformOffset = baseformOffset(info);
        if(baseformOffset == 0x1FFFF)
            return m.surface;
        final int baseformLength = baseformLength(info);
        
        return text(baseformOffset, baseformLength);
    }

    private static int baseformOffset(long info) {
        return (int)(info & 0x1FFFF);
    }

    private static int baseformLength(long info) {
        return (int)((info >> 38) & 0xF);
    }

    private static String reading_pronunciation(long info) {
        return text(rpOffset(info), rpLength(info));
    }

    private static int rpOffset(long info) {
        return (int)((info >> 17) & 0x1FFFFF);
    }

    private static int rpLength(long info) {
        return (int)((info >> 42) & 0x3F);
    }

    private static String text(int start, int length) {
        try {
            return new String(data, start*2, length*2, "UTF-16BE");
        } catch (java.io.UnsupportedEncodingException ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * instance
     **/
    public final String baseform;
    public final String reading;
    public final String pronunciation;

    public FeatureEx(Morpheme m) {
        final long info = info(m.morphemeId);
        baseform = baseform(info, m);

        final String rp = reading_pronunciation(info);
        final int i = rp.indexOf(",");
        if(i==-1) {
            reading = pronunciation = rp;
        } else {
            reading = rp.substring(0, i);
            pronunciation = rp.substring(i+1);
        } 
    }
}