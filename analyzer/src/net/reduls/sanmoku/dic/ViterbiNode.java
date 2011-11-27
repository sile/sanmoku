package net.reduls.sanmoku.dic;

public final class ViterbiNode {
    public int cost;
    public ViterbiNode prev = null;
    
    public final int start;
    private final int length_posId_isSpace;
    public final int morphemeId;

    public ViterbiNode(int start, short length, short wordCost, short posId, boolean isSpace, 
                       int morphemeId) {
        this.cost = wordCost;
        
        this.start = start;
        this.length_posId_isSpace = (length << 17) + (posId << 1) + (isSpace ? 1 : 0);

        this.morphemeId = morphemeId;
    }

    public short length() { return (short)(length_posId_isSpace >> 17); }
    public short posId() { return (short)((length_posId_isSpace >> 1) & 0x0000FFFF); }
    public boolean isSpace() { return (length_posId_isSpace & 1)==1; }
    
    public static ViterbiNode makeBOSEOS() {
        return new ViterbiNode(0, (short)0, (short)0, (short)0, false, 0);
    }
}