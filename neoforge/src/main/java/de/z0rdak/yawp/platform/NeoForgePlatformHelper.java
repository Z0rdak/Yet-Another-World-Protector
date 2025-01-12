package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.platform.services.IPlatformHelper;
import net.neoforged.fml.ModList;
import net.neoforged.fml.loading.FMLLoader;

public class NeoForgePlatformHelper implements IPlatformHelper {

    @Override
    public String getPlatformName() {
        return "NeoForge";
    }

    @Override
    public boolean isModLoaded(String modId) {
        return ModList.get().isLoaded(modId);
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
        return !FMLLoader.isProduction();
    }
}