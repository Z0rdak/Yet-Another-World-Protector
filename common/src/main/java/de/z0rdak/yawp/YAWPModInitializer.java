package de.z0rdak.yawp;

public interface YAWPModInitializer {

    void registerCommands();
    void initServerInstance();
    void loadRegionData();
    void addDimKeyOnPlayerLogin();
    void addDimKeyOnDimensionChange();
    void registerConfig();
}
