package net.reduls.sanmoku.dic;

public final class ViterbiNode {
    public int cost;
    public ViterbiNode prev = null;
    
    public final int start;
    private final int length_posId;
    public final int morphemeId;

    public ViterbiNode(int start, short length, short wordCost, short posId, int morphemeId) {
        this.cost = wordCost;
        
        this.start = start;
        this.length_posId = (length << 16) + posId;

        this.morphemeId = morphemeId;
    }

    public short length() { return (short)(length_posId >> 16); }
    public short posId() { return (short)(length_posId & 0x0000FFFF); }
    
    public static ViterbiNode makeBOSEOS() {
        return new ViterbiNode(0, (short)0, (short)0, (short)0, 0);
    }
}