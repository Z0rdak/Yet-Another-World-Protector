package de.z0rdak.yawp.api.visualization;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.TextDisplayProperties;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.commands.data.EntityDataAccessor;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Display;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;

import java.util.Optional;

public final class VisualizationUtil {

    private VisualizationUtil() {}

    public static void updateDisplayBlock(Entity blockDisplayEntity, ResourceLocation blockRl) {
        if (!(blockDisplayEntity instanceof Display.BlockDisplay)) {
            throw new IllegalArgumentException("BlockDisplay entity is not a Display.BlockDisplay");
        }
        var entityDataAccessor = new EntityDataAccessor(blockDisplayEntity);
        CompoundTag entityTag = entityDataAccessor.getData();
        CompoundTag blockState = entityTag.getCompound("block_state");
        blockState.putString("Name", blockRl.toString());
        entityTag.put("block_state", blockState);
        try {
            entityDataAccessor.setData(entityTag);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error("Should not happend - what did you do?!", e);
            throw new RuntimeException(e);
        }
    }

    public static void updateDisplayGlow(Entity blockDisplayEntity, boolean glow) {
        if (!(blockDisplayEntity instanceof Display.BlockDisplay)) {
            throw new IllegalArgumentException("BlockDisplay entity is not a Display.BlockDisplay");
        }
        var entityDataAccessor = new EntityDataAccessor(blockDisplayEntity);
        CompoundTag entityTag = entityDataAccessor.getData();
        entityTag.putBoolean("Glowing", glow);
        try {
            entityDataAccessor.setData(entityTag);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error("Should not happend - what did you do?!", e);
            throw new RuntimeException(e);
        }
    }

    public static void updateDisplayLightLevel(Entity blockDisplayEntity, int lightLevel) {
        if (!(blockDisplayEntity instanceof Display.BlockDisplay)) {
            throw new IllegalArgumentException("BlockDisplay entity is not a Display.BlockDisplay");
        }
        var entityDataAccessor = new EntityDataAccessor(blockDisplayEntity);
        CompoundTag entityTag = entityDataAccessor.getData();
        var brightnessTag = new CompoundTag();
        brightnessTag.putInt("sky", lightLevel);
        brightnessTag.putInt("block", lightLevel);
        entityTag.put("brightness", brightnessTag);
        try {
            entityDataAccessor.setData(entityTag);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error("Should not happend - what did you do?!", e);
            throw new RuntimeException(e);
        }
    }

    public static void initTextDisplayProperties(Entity blockDisplayEntity, String regionName, TextDisplayProperties properties) {
        var entityDataAccessor = new EntityDataAccessor(blockDisplayEntity);
        try {
            var entityTag = buildTeleportAnchorTextDisplayTag(regionName, properties);
            entityDataAccessor.setData(entityTag);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error("Should not happend - what did you do?!", e);
            throw new RuntimeException(e);
        }
    }

    public static void initBlockDisplayProperties(Entity blockDisplayEntity, String regionName, BlockDisplayProperties properties) {
        var entityDataAccessor = new EntityDataAccessor(blockDisplayEntity);
        try {
            var blockDisplayTag = buildBlockDisplayTag(regionName, properties);
            CompoundTag existingTag = entityDataAccessor.getData();
            existingTag.put("block_state", blockDisplayTag.getCompound("block_state"));
            existingTag.put("brightness", blockDisplayTag.getCompound("brightness"));
            existingTag.put("data", blockDisplayTag.getCompound("data"));
            existingTag.putString("id", blockDisplayTag.getString("id"));
            existingTag.putBoolean("Glowing", blockDisplayTag.getBoolean("Glowing"));
            entityDataAccessor.setData(existingTag);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error("Should not happend - what did you do?!", e);
            throw new RuntimeException(e);
        }
    }

    public static void updateDisplayProperties(Entity blockDisplayEntity, BlockDisplayProperties properties) {
        var entityDataAccessor = new EntityDataAccessor(blockDisplayEntity);
        CompoundTag entityTag = entityDataAccessor.getData();
        CompoundTag blockState = entityTag.getCompound("block_state");
        blockState.putString("Name", properties.blockRl().toString());
        entityTag.put("block_state", blockState);
        entityTag.putBoolean("Glowing", properties.hasGlow());
        var brightnessTag = new CompoundTag();
        brightnessTag.putInt("sky", properties.lightLevel());
        brightnessTag.putInt("block", properties.lightLevel());
        entityTag.put("brightness", brightnessTag);
        try {
            entityDataAccessor.setData(entityTag);
        } catch (CommandSyntaxException e) {
            Constants.LOGGER.error("Should not happend - what did you do?!", e);
            throw new RuntimeException(e);
        }
    }

    /**
     *
     * @param regionName a marker, stored in custom entity data for identification later
     */
    public static Optional<Entity> createTextDisplayEntity(ServerLevel level, String regionName, BlockPos pos, TextDisplayProperties displayProperties) {
        var textDisplay = EntityType.BLOCK_DISPLAY.create(level, (e) -> {
            VisualizationUtil.initTextDisplayProperties(e, regionName, displayProperties);
        }, pos, EntitySpawnReason.COMMAND, false, false);
        return textDisplay == null ? Optional.empty() : Optional.of(textDisplay);
    }

    /**
     *
     * @param regionName a marker, stored in custom entity data for identification later
     */
    public static Optional<Entity> createBlockDisplayEntity(ServerLevel level, String regionName, BlockPos pos, BlockDisplayProperties displayProperties) {
        Entity blockDisplay = EntityType.BLOCK_DISPLAY.create(level, (e) -> {
            VisualizationUtil.initBlockDisplayProperties(e, regionName, displayProperties);
            e.moveTo(pos.getX(), pos.getY(), pos.getZ(), 0, 0);
        }, pos, EntitySpawnReason.COMMAND, false, false);
        return blockDisplay == null ? Optional.empty() : Optional.of(blockDisplay);
    }

    /**
     * SEE: https://minecraft.wiki/w/Display#Data_values
     */
    public static CompoundTag buildTeleportAnchorTextDisplayTag(String regionName, TextDisplayProperties properties) {
        var textDisplayTag = new CompoundTag();

        // General Entity Tags
        ResourceLocation blockDisplayRl = ResourceLocation.withDefaultNamespace("text_display");
        textDisplayTag.putString("id", blockDisplayRl.toString());
        // textDisplayTag.putBoolean("Glowing", properties.hasGlow());

        // custom data
        CompoundTag data = new CompoundTag();
        data.putString("yawp_display", "text");
        data.putString("region", regionName);
        data.putString("tpAnchor", properties.getText());
        textDisplayTag.put("data", data);

        // Display Entity Tags
        var brightnessTag = new CompoundTag();
        brightnessTag.putInt("sky", 15);
        brightnessTag.putInt("block", 15);
        textDisplayTag.put("brightness", brightnessTag);
        textDisplayTag.putString("billboard", "center");

        // Text Display Entity Tags
        var text = Component.literal(properties.toString());
        // TODO: text.getString() ?
        textDisplayTag.putString("text", text.toString());
        textDisplayTag.putString("alignment", "center");
        textDisplayTag.putInt("background", TextDisplayProperties.DEFAULT_BACKGROUND);

        return textDisplayTag;
    }

    public static CompoundTag buildBlockDisplayTag(String regionName, BlockDisplayProperties properties) {
        var blockDisplayTag = new CompoundTag();

        // General Entity Tags
        ResourceLocation blockDisplayRl = ResourceLocation.withDefaultNamespace("block_display");
        blockDisplayTag.putString("id", blockDisplayRl.getPath());
        blockDisplayTag.putBoolean("Glowing", properties.hasGlow());

        // custom data
        CompoundTag data = new CompoundTag();
        data.putString("yawp_display", "block");
        data.putString("region", regionName);
        blockDisplayTag.put("data", data);

        // Display Entity Tags
        var brightnessTag = new CompoundTag();
        brightnessTag.putInt("sky", properties.lightLevel());
        brightnessTag.putInt("block", properties.lightLevel());
        blockDisplayTag.put("brightness", brightnessTag);

        // Block Display Tags
        var blockstateTag = new CompoundTag();
        blockstateTag.putString("Name", properties.blockRl().toString());
        blockDisplayTag.put("block_state", blockstateTag);

        return blockDisplayTag;
    }
}
