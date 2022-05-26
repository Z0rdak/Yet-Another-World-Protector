package de.z0rdak.regionshield.handler;

import de.z0rdak.regionshield.core.stick.FlagStick;
import de.z0rdak.regionshield.util.StickType;
import de.z0rdak.regionshield.util.StickUtil;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.regionshield.util.StickUtil.STICK;

public class FlagStickHandler {

    public static void onCycleFlagStick(ItemStack flagStickItem) {
        CompoundNBT nbt = flagStickItem.getTag();
        FlagStick flagStick = new FlagStick(nbt.getCompound(STICK));
        // cycle mode
        flagStick.cycleMode();
        // update stick name
        flagStickItem.getTag().put(STICK, flagStick.serializeNBT());
        StickUtil.setStickName(flagStickItem, StickType.REGION_STICK);
    }
}
