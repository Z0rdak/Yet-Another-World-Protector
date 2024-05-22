package de.z0rdak.yawp.core.flag;

public interface IFlagContainer {

    void put(IFlag flag);

    boolean contains(String flag);

    FlagState flagState(String flagName);

    void updateFlag(IFlag flag);

    void toggleFlag(String flag, boolean enable);
}
