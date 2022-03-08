package de.z0rdak.regionshield.util;

import de.z0rdak.regionshield.core.stick.FlagStick;
import de.z0rdak.regionshield.core.stick.MarkerStick;
import de.z0rdak.regionshield.core.stick.RegionStick;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.text.IFormattableTextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String STICK_TYPE = "stick_type";
    public static final String STICK = "stick";

    public static void applyEnchantmentGlint(ItemStack item) {
        CompoundNBT dummy = new CompoundNBT();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        ListNBT enchantmentList = new ListNBT();
        enchantmentList.add(dummy);
        item.addTagElement("Enchantments", enchantmentList);
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param type  stick type to create
     * @param dim   dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, StickType type, RegistryKey<World> dim) {
        CompoundNBT itemTag = new CompoundNBT();
        if (stick.hasTag()) {
            itemTag = stick.getTag();
        }
        if (itemTag != null) {
            switch (type) {
                case MARKER:
                    itemTag.put(STICK, new MarkerStick(dim).serializeNBT());
                    break;
                case FLAG_STICK:
                    itemTag.put(STICK, new FlagStick().serializeNBT());
                    break;
                case REGION_STICK:
                    itemTag.put(STICK, new RegionStick().serializeNBT());
                    break;
                default:
                    break;
            }
        }
    }

    public static boolean isStick(ItemStack itemStack) {
        return itemStack.getItem().getDescriptionId().equals(Items.STICK.getDescriptionId());
    }

    // TODO: fixme
    public static StickType getStickType(ItemStack stick) {
        if (stick.getTag() != null && stick.hasTag()) {
            if (stick.getTag().contains(STICK)) {
                return StickType.of(stick.getTag().getCompound(STICK).getString(STICK_TYPE));
            }
        } else {
            return StickType.UNKNOWN;
        }
        return StickType.UNKNOWN;
    }

    public static void setStickName(ItemStack stick, StickType type) {
        String displayName = "";
        switch (type) {
            case REGION_STICK:
                displayName = TextFormatting.GREEN + type.stickName;
                break;
            case FLAG_STICK:
                displayName = TextFormatting.AQUA + type.stickName;
                break;
            case MARKER:
                MarkerStick marker = new MarkerStick(stick.getTag().getCompound(STICK));
                String validFlag = marker.isValidArea() ? (TextFormatting.GREEN + "*" + TextFormatting.GOLD) : "";
                displayName = TextFormatting.GOLD + type.stickName + " (" + marker.getAreaType().areaType + "" + validFlag + ")";
                break;
            default:
                break;
        }
        stick.setHoverName(new StringTextComponent(displayName));
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        switch (type) {
            case REGION_STICK:
                setToolTip(stick, getRegionStickToolTip());
                break;
            case FLAG_STICK:
                setToolTip(stick, getFlagStickToolTip());
                break;
            case MARKER:
                setToolTip(stick, getMarkerToolTip());
                break;
            default:
                break;
        }
    }

    public static void setToolTip(ItemStack stack, ListNBT loreNbt) {
        stack.getOrCreateTagElement("display").put("Lore", loreNbt);
    }

    // TODO
    private static ListNBT getMarkerToolTip() {
        ListNBT lore = new ListNBT();
        StringNBT simple1 = buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.1"), "#ff0020");
        StringNBT simple2 = buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.marker.simple.2"), "#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    // TODO
    private static ListNBT getFlagStickToolTip() {
        ListNBT lore = new ListNBT();
        StringNBT simple1 = buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.flag-stick.simple.1"), "#ff0020");
        StringNBT simple2 = buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.flag-stick.simple.2"), "#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    // TODO
    private static ListNBT getRegionStickToolTip() {
        ListNBT lore = new ListNBT();
        StringNBT simple1 = buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.region-stick.simple.1"), "#ff0020");
        StringNBT simple2 = buildLoreTextLine(new TranslationTextComponent("help.tooltip.stick.region-stick.simple.2"), "#ff0010");
        lore.add(simple1);
        lore.add(simple2);
        return lore;
    }

    private static StringNBT buildLoreTextLine(String text, String hexColor) {
        return StringNBT.valueOf("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static StringNBT buildLoreTextLine(IFormattableTextComponent text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
