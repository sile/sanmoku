package net.reduls.sanmoku.dic;

import java.io.DataInputStream;
import net.reduls.sanmoku.util.Misc;

public final class Char {
    public static final class Category {
        public final int id;
        public final boolean invoke;
        public final boolean group;
        public final byte length;

        public Category(int id, boolean invoke, boolean group, byte length) {
            this.id = id;
            this.invoke = invoke;
            this.group = group;
            this.length = length;
        }
    }
    
    private final static Category[] charCategorys;
    private final static byte[] charInfos;

    static {
        DataInputStream in = Misc.openDictionaryDataAsDIS("category.bin");
        
        final int charCategoryNum = Misc.readInt(in);
        charCategorys = new Category[charCategoryNum];
        for(int i=0; i < charCategoryNum; i++)
            charCategorys[i] = new Category(i,
                                            Misc.readByte(in)==1,
                                            Misc.readByte(in)==1,
                                            Misc.readByte(in));
        Misc.close(in);
        
        charInfos = Misc.readBytesFromFile("code.bin", 3);
    }

    public static final Category category(char c) {
        return charCategorys[charInfos[(int)(c)*3]];
    }

    public static final boolean isCompatible(char c1, char c2) {
        return (compatibleMask(c1) & compatibleMask(c2)) != 0;
    }

    private static final short compatibleMask(char c) {
        return (short)((charInfos[(int)(c)*3+1]<<8) | charInfos[(int)(c)*3+2]&0xFF);
    }
}
