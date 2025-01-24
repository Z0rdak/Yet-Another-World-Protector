package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.platform.services.IPlatformHelper;
import net.fabricmc.loader.api.FabricLoader;

public class FabricPlatformHelper implements IPlatformHelper {

    @Override
    public String getPlatformName() {
        return "Fabric";
    }

    @Override
    public boolean isModLoaded(String modId) {

        return FabricLoader.getInstance().isModLoaded(modId);
    }

    @Override
    public boolean isWorldEditLoaded() {
        return this.isModLoaded("worldedit");
    }

    @Override
    public boolean isWorldEditCuiLoaded() {
        return this.isModLoaded("worldedit-cui");
    }

    @Override
    public boolean isJourneyMapLoaded() {
        return this.isModLoaded("journeymap");
    }
    
    @Override
    public boolean isDevelopmentEnvironment() {

        return FabricLoader.getInstance().isDevelopmentEnvironment();
    }
}