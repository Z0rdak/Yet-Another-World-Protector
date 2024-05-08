package de.z0rdak.yawp;

import de.z0rdak.yawp.config.ConfigRegistry;
import net.minecraft.network.chat.Component;
import net.neoforged.fml.IExtensionPoint;
import net.neoforged.fml.ModLoadingContext;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.loading.FMLEnvironment;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(YetAnotherWorldProtector.MODID)
public class YetAnotherWorldProtector {
    public static final String MODID = "yawp";
    public static final String MODID_LONG = "Yet Another World Protector";
    public static final Logger LOGGER = LogManager.getLogger("YAWP");

    public YetAnotherWorldProtector() {
        if (FMLEnvironment.dist.isDedicatedServer()) {
            try
            {
               ConfigRegistry.register();
            }
            catch (Exception e)
            {
                throw new RuntimeException(e);
            }
        } else {
            LOGGER.info(Component.translatableWithFallback("loading.client.info", "You are loading %s on a client. %s is a server-side only mod. The client only provides Internationalization.",MODID_LONG, MODID.toUpperCase()).getString());
        }

        //Make sure the mod being absent on the other network side does not cause the client to display the server as incompatible
        ModLoadingContext.get().registerExtensionPoint(IExtensionPoint.DisplayTest.class, () -> new IExtensionPoint.DisplayTest(() -> IExtensionPoint.DisplayTest.IGNORESERVERONLY, (s, b) -> true));
    }
}
