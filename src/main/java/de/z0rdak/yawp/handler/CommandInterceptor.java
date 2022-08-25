package de.z0rdak.yawp.handler;

import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.command.CommandSource;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraftforge.event.CommandEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;

public class CommandInterceptor {

    @SubscribeEvent
    public void handleModCommandPermission(CommandEvent event){
        CommandSource src = event.getParseResults().getContext().getSource();
        if (src.getEntity() == null) {
            // server console or command block
            // TODO: Check both and add config for command block
            YetAnotherWorldProtector.LOGGER.debug(src.getDisplayName().toString());
            YetAnotherWorldProtector.LOGGER.debug(src.getTextName());
        } else {
            try {
                // most likely player executing command
                // TODO: handle permissions
                if (src.getEntity() instanceof PlayerEntity){
                    ServerPlayerEntity player = src.getPlayerOrException();
                }
            } catch (CommandSyntaxException e) {
                // TODO: Logging
            }

        }
    }
}