package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.platform.services.IPlatformHelper;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.loading.FMLLoader;
import org.apache.commons.io.output.ThresholdingOutputStream;

public class ForgePlatformHelper implements IPlatformHelper {

    @Override
    public String getPlatformName() {

        return "Forge";
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