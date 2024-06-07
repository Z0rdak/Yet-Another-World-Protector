package de.z0rdak.yawp.util;

import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.stick.AbstractStick;
import de.z0rdak.yawp.core.stick.MarkerStick;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtList;
import net.minecraft.nbt.NbtString;
import net.minecraft.registry.RegistryKey;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.util.Formatting;
import net.minecraft.world.World;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;

import static net.minecraft.util.Formatting.*;

public final class StickUtil {

    private StickUtil() {
    }

    public static final String MARKED_BLOCKS = "blocks";
    public static final String VALID_AREA = "valid";
    public static final String AREA_TYPE = "type";
    public static final String DIM = "dim";
    public static final String TP_POS = "tp_pos";
    public static final String IS_TP_SET = "is_tp_set";
    public static final String STICK_TYPE = "stick_type";
    public static final String STICK_ID = "stick-id";
    public static final String STICK = "stick";

    private static final String MARKED_BLOCK_INDICATOR = "X";

    public static void applyEnchantmentGlint(ItemStack item) {
        NbtCompound dummy = new NbtCompound();
        dummy.putString("id", "");
        dummy.putInt("lvl", 1);
        NbtList enchantmentList = new NbtList();
        enchantmentList.add(dummy);
        item.setSubNbt("Enchantments", enchantmentList);
    }

    /**
     * Set init (default) nbt value for sticks
     *
     * @param stick stick item
     * @param type  stick type to create
     * @param dim   dimension tag to set for sticks
     */
    public static void initStickTag(ItemStack stick, StickType type, RegistryKey<World> dim) {
        NbtCompound itemTag = stick.hasNbt() ? stick.getNbt() : new NbtCompound();
        if (itemTag != null) {
            if (Objects.requireNonNull(type) == StickType.MARKER) {
                NbtCompound compoundNBT = new MarkerStick(dim).serializeNBT();
                itemTag.put(STICK, compoundNBT);
                stick.setNbt(itemTag);
            }
        }
    }

    public static ItemStack initMarkerNbt(ItemStack stack, StickType type, RegistryKey<World> dim) {
        stack.setCount(1);
        initStickTag(stack, type, dim);
        setStickName(stack, type);
        setStickToolTip(stack, type);
        applyEnchantmentGlint(stack);
        return stack;
    }

    public static boolean isVanillaStick(ItemStack itemStack) {
        return ItemStack.areItemsEqual(itemStack, Items.STICK.getDefaultStack());
    }

    public static AbstractStick getStick(ItemStack stick) throws StickException {
        if (stick.getNbt() != null && stick.hasNbt()) {
            if (stick.getNbt().contains(STICK)) {
                NbtCompound stickNbt = stick.getNbt().getCompound(STICK);
                StickType type = StickType.of(stickNbt.getString(STICK_TYPE));
                switch (type) {
                    case MARKER:
                        return new MarkerStick(stickNbt);
                    case UNKNOWN:
                    default:
                        throw new StickException("Unknown stick type: '" + type + "'!");
                }
            }
        }
        throw new StickException("Invalid or missing NBT data for Stick '" + stick.toHoverableText().getString() + "'!");
    }

    public static StickType getStickType(ItemStack stick) {
        if (stick.getNbt() != null && stick.hasNbt()) {
            if (stick.getNbt().contains(STICK)) {
                NbtCompound stickNbt = stick.getNbt().getCompound(STICK);
                if (stickNbt.contains(STICK_TYPE)) {
                    return StickType.of(stickNbt.getString(STICK_TYPE));
                }
            }
        }
        return StickType.UNKNOWN;
    }
    private static final String UNMARKED_BLOCK_INDICATOR = "#";

    public static NbtCompound getStickNBT(ItemStack stick) {
        if (stick.getNbt() != null && stick.hasNbt()
                && stick.getNbt().contains(STICK)) {
            return stick.getNbt().getCompound(STICK);
        } else {
            return null;
        }
    }
    private static final String TP_POS_INDICATOR = "TP";
    private static final Formatting MARKED_BLOCK_COLOR = GREEN;
    private static final Formatting UNMARKED_BLOCK_COLOR = RED;
    private static final Formatting UNMARKED_POS_COLOR = AQUA;

    @Nullable
    public static IMarkableArea getMarkedArea(ItemStack stick) {
        if (isVanillaStick(stick) && isMarker(stick)) {
            NbtCompound stickNBT = StickUtil.getStickNBT(stick);
            if (stickNBT != null) {
                MarkerStick marker = new MarkerStick(stickNBT);
                if (!marker.isValidArea()) {
                    return null;
                }
                return LocalRegions.areaFrom(marker);
            }
        }
        return null;
    }

    public static boolean isMarker(ItemStack stick) {
        return getStickType(stick) == StickType.MARKER;
    }

    public static void setStickName(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            MarkerStick marker = new MarkerStick(getStickNBT(stick));
            boolean isTpPosSet = marker.getTeleportPos() != null;
            MutableText markerIndicators = buildRegionMarkerIndicators(marker)
                    .append(" ")
                    .append(buildTpPosIndicator(isTpPosSet));
            MutableText markerHoverName = buildStickName(marker)
                    .append(" ")
                    .append(markerIndicators);
            stick.setCustomName(markerHoverName);
        }
    }

    private static MutableText buildStickName(MarkerStick marker) {
        MutableText stickName = Text.literal(marker.getStickType().stickName).formatted(GOLD);
        MutableText areaType = Text.literal(" (").append(marker.getAreaType().areaType).append(")");
        return stickName.append(areaType);
    }

    /**
     * @param isMarked
     * @return [X] or [#]
     */
    private static MutableText buildMarkerIndicator(boolean isMarked) {
        String indicator = isMarked ? MARKED_BLOCK_INDICATOR : UNMARKED_BLOCK_INDICATOR;
        Formatting color = isMarked ? MARKED_BLOCK_COLOR : UNMARKED_BLOCK_COLOR;
        MutableText indicatorComp = Text.literal(indicator).formatted(color);
        MutableText closedResetComp = Text.literal("]").formatted(RESET);
        return Text.literal("[").append(indicatorComp).append(closedResetComp);
    }

    private static MutableText buildTpPosIndicator(boolean isMarked) {
        Formatting color = isMarked ? MARKED_BLOCK_COLOR : UNMARKED_POS_COLOR;
        MutableText indicatorComp = Text.literal(TP_POS_INDICATOR).formatted(color);
        MutableText closedResetComp = Text.literal("]").formatted(RESET);
        return Text.literal("[").append(indicatorComp).append(closedResetComp);
    }

    /**
     * RegionMarker [x][x] [TP]
     *
     * @param marker
     * @return
     */
    private static MutableText buildRegionMarkerIndicators(MarkerStick marker) {
        MutableText regionMarkerIndicators = Text.literal("");
        int maxBlocks = marker.getAreaType().maxBlocks;
        int amountUnmarked = maxBlocks - marker.getMarkedBlocks().size();
        for (int i = 0; i < marker.getMarkedBlocks().size(); i++) {
            regionMarkerIndicators.append(buildMarkerIndicator(true));
        }
        for (int i = 0; i < amountUnmarked; i++) {
            regionMarkerIndicators.append(buildMarkerIndicator(false));
        }
        return regionMarkerIndicators;
    }

    public static void setStickToolTip(ItemStack stick, StickType type) {
        if (Objects.requireNonNull(type) == StickType.MARKER) {
            setToolTip(stick, getMarkerToolTip());
        }
    }

    public static void setToolTip(ItemStack stack, NbtList loreNbt) {
        stack.getOrCreateSubNbt("display").put("Lore", loreNbt);
    }

    public static boolean hasNonNullTag(ItemStack itemStack) {
        return itemStack.hasNbt() && itemStack.getNbt() != null;
    }

    private static NbtList getMarkerToolTip() {
        NbtList lore = new NbtList();
        lore.add(buildLoreTextLine(Text.translatableWithFallback("help.tooltip.stick.marker.simple.1", "Used to mark a new region."), "#ff4d4d"));
        lore.add(buildLoreTextLine(Text.translatableWithFallback("help.tooltip.stick.marker.simple.2", "Keep the Region Marker in your hand while creating a region!"), "#ff4d4d"));
        lore.add(buildLoreTextLine(Text.literal(ITALIC + "").append(Text.translatableWithFallback("help.tooltip.stick.marker.simple.3", "Mark a (Cuboid) region by right-clicking the diagonal opposite corner blocks.")), "#808080"));
        lore.add(buildLoreTextLine(Text.literal(ITALIC + "").append(Text.translatableWithFallback("help.tooltip.stick.marker.simple.4", "Set a region teleport position by shift-right-clicking a block.")), "#808080"));
        return lore;
    }

    private static NbtElement buildLoreTextLine(String text, String hexColor) {
        return NbtString.of("{\"text\":\"" + text + "\", \"color\":\"" + hexColor + "\"}");
    }

    private static NbtElement buildLoreTextLine(MutableText text, String hexColor) {
        return buildLoreTextLine(text.getString(), hexColor);
    }

}
